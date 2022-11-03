(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                                Yan Dong                                *)
(*                                                                        *)
(*   Copyright 2022 Yan Dong                                              *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typing for query plans *)

open Longident
open Asttypes
open Parsetree
open Ast_helper
open Types
open Typedtree
open Ctype

let scope_check scopes exp =
  let super = Tast_iterator.default_iterator in
  let current_scope = List.hd scopes in
  let outer_scopes = List.tl scopes in
  let current = ref false in
  let outer = ref false in
  let expr self exp =
    match exp.exp_desc with
    | Texp_ident (Path.Pident id, _, _) ->
        let scope = Ident.scope id in
        if scope = current_scope then
          current := true
        else if List.mem scope outer_scopes then
          outer := true
    | _ -> super.expr self exp
  in
  let iterator = {super with expr} in
  iterator.expr iterator exp;
  !current || not !outer

let rec check_aggregate ~scopes env exp action =
  let super = Tast_iterator.default_iterator in
  let expr self exp =
    match exp.exp_desc with
    | Texp_aggregate (f, e) when scope_check scopes e ->
        action exp;
        check_no_aggregate ~scopes env f;
        check_no_aggregate ~scopes env e;
        super.expr self f
    | _ -> super.expr self exp
  in
  let iterator = {super with expr} in
  iterator.expr iterator exp

and check_no_aggregate ~scopes env exp =
  check_aggregate ~scopes env exp
    (fun _ -> raise (Typecore.Error (
      exp.exp_loc, env, Invalid_use_of_aggregate)))

let col_gen = ref 0
let incr_col () = col_gen := !col_gen + 1

let col_of_exp exp =
  let loc = exp.exp_loc in
  let name = match exp.exp_desc with
  | Texp_ident (_, lid, _) ->
      "__col_" ^ String.concat "_" (flatten lid.txt)
  | _ ->
      incr_col();
      "__col_" ^ string_of_int !col_gen
  in
  let pat = Pat.var (mkloc name loc) in
  let exp =
    { exp with
      exp_desc =
        Texp_ident
          (Path.Pident (Ident.create_local name),
            mkloc (Lident name) loc,
            {val_type = exp.exp_type; val_kind = Val_reg;
            val_loc = loc; val_attributes = [];
            val_uid = Types.Uid.mk ~current_unit:(Env.get_unit_name ());
            })
    } in
  (pat, exp)

let expand_product exp =
  match exp.exp_desc with
  | Texp_tuple es ->
      let pats, cols = List.split (List.map col_of_exp es) in
      (es, cols, pats, { exp with exp_desc = Texp_tuple cols })
  | Texp_record { fields; representation; extended_expression } ->
      let exps = ref [] in
      let cols = ref [] in
      let pats = ref [] in
      let fields = Array.map (function
        | label, Kept t -> label, Kept t
        | label, Overridden (lid, exp) ->
            let pat, col = col_of_exp exp in
            exps := exp :: !exps;
            cols := col :: !cols;
            pats := pat :: !pats;
            label, Overridden (lid, col))
        fields
      in
      List.rev !exps, List.rev !cols, List.rev !pats,
      { exp with exp_desc =
        Texp_record {fields; representation; extended_expression}
      }
  | _ ->
      let pat, col = col_of_exp exp in
      ([exp], [col], [pat], col)

let expand_aggregate ~scopes exp =
  let current_scope = List.hd scopes in
  let super = Tast_mapper.default in
  let aggs = ref [] in
  let expr self exp =
    match exp.exp_desc with
    | Texp_aggregate (func, arg)
      when scope_check scopes arg ->
        let argpat, argcol = col_of_exp arg in
        let retpat, retcol = col_of_exp exp in
        aggs := (arg, argcol, argpat, Some func, retcol, retpat) :: !aggs;
        retcol
    | Texp_ident (Path.Pident id, _, _)
      when Ident.scope id = current_scope ->
        let pat, col = col_of_exp exp in
        aggs := (exp, col, pat, None, col, pat) :: !aggs;
        col
    | _ -> super.expr self exp
  in
  let mapper = {super with expr} in
  let exp = mapper.expr mapper exp in
  let aggs = List.rev !aggs in
  let funcs = List.map (fun (func, _, _, _, _, _) -> func) aggs in
  let arglist = List.map (fun (_, arg, _, _, _, _) -> arg) aggs in
  let argcols = List.map (fun (_, _, col, _, _, _) -> col) aggs in
  let argpats = List.map (fun (_, _, _, pat, _, _) -> pat) aggs in
  let retcols = List.map (fun (_, _, _, _, col, _) -> col) aggs in
  let retpats = List.map (fun (_, _, _, _, _, pat) -> pat) aggs in
  (funcs, arglist, argcols, argpats, retcols, retpats, exp)

let build_plan ~loc env se =
  let old_env = env in
  let scope = create_scope () in
  let child =
    ref { plan_desc = Tplan_null;
          plan_loc = loc;
          plan_env = env;
          plan_cardinality = One;
          plan_patterns =
            [Ast_helper.Pat.construct ~loc
              (mkloc (Longident.Lident "()") loc) None];
        } in
  (* handle FROM *)
  let env =
    match se.se_from with
    | None -> env
    | Some se_from ->
        let rec aux se_from =
          match se_from.psrc_desc with
          | Psrc_exp (e, s) ->
              let tys = List.map (fun _ -> newvar ()) s in
              let ty = match tys with
                | [ty] -> ty
                | tys -> newty (Ttuple tys)
              in
              let ty_src =
                let lid = Longident.(Ldot (Lident "SelectML", "src")) in
                let path, decl = Env.lookup_type ~loc:e.pexp_loc lid env in
                assert (List.length decl.type_params = 1);
                newconstr path [ty]
              in
              let exp = Typecore.type_expect env e
                (Typecore.mk_expected ty_src) in
              let plan =
                { plan_loc = se_from.psrc_loc;
                  plan_desc = Tplan_source exp;
                  plan_env = env;
                  plan_cardinality = Many;
                  plan_patterns =
                    List.map
                      (fun s -> Ast_helper.Pat.var ~loc:se_from.psrc_loc s)
                      s;
                } in
              plan, s, tys
          | Psrc_join (s1, s2) ->
              let pl1, names1, tys1 = aux s1 in
              let pl2, names2, tys2 = aux s2 in
              let names = names1 @ names2 in
              let tys = tys1 @ tys2 in
              let plan =
                { plan_loc = se_from.psrc_loc;
                  plan_desc = Tplan_product (pl1, pl2);
                  plan_env = env;
                  plan_cardinality = Many;
                  plan_patterns = pl1.plan_patterns @ pl2.plan_patterns
                } in
              plan, names, tys
        in
        let plan, names, tys = aux se_from in
        let rec check_dup = function
        | [] -> ()
        | s :: l ->
            if List.exists (fun s' -> s.txt = s'.txt) l then
              raise (Typecore.(Error (
                s.loc, env, Multiply_bound_variable s.txt)))
            else check_dup l
        in
        check_dup names;
        child := plan;
        let src_env = List.fold_left2
          (fun env s ty ->
            let id = Ident.create_scoped ~scope s.txt in
            let desc =
              { val_type = ty; val_kind = Val_reg;
                val_attributes = [];
                val_loc = s.loc;
                val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
              }
            in
            Env.add_value id desc env)
          env names tys in
        src_env
  in

  begin_se_scope scope;
  let scopes = se_scopes () in

  (* handle WHERE *)
  begin match se.se_where with
  | None -> ()
  | Some se_where ->
      let exp = Typecore.type_exp env se_where in
      check_no_aggregate ~scopes env exp;
      child :=
        { plan_loc = exp.exp_loc;
          plan_desc = Tplan_filter (!child, exp);
          plan_env = env;
          plan_cardinality =
            (match !child.plan_cardinality with
            | Zero | One -> Zero | Many -> Many);
          plan_patterns = !child.plan_patterns
        }
  end;

  (* handle SELECT *)
  let no_aggregate = ref true in
  let no_extra_project = ref true in
  let sel_exp =
    let exp = Typecore.type_exp env se.se_select in
    check_aggregate ~scopes env exp
      (fun _ -> no_aggregate := false);
    exp
  in
  if Option.is_some se.se_groupby then
    no_aggregate := false;
  if Option.is_some se.se_having then
    no_extra_project := false;
  if List.length se.se_orderby > 0 then
    no_extra_project := false;

  let aux sexp =
    let exp = Typecore.type_exp env sexp in
    check_aggregate ~scopes env exp
      (fun _ -> no_aggregate := false);
    exp
  in
  let hav_exp = match se.se_having with
    | Some sexp when !no_aggregate -> Some (aux sexp)
    | _ -> None
  in
  let ord_exps = match se.se_orderby with
    | _ :: _ when !no_aggregate ->
        List.map (fun (sexp, _) -> aux sexp) se.se_orderby
    | _ -> []
  in
  let ord_dirs =
    List.map
      (function
      | _, PAscending -> TAscending
      | _, PDescending -> TDescending
      | _, PUsing sexp -> TUsing (Typecore.type_exp old_env sexp))
    se.se_orderby
  in


  let module TastOrd = struct
    type t = expression
    let compare t1 t2 = Stdlib.compare t1 t2
  end in
  let module AstOrd = struct
    type t = Parsetree.expression
    let compare t1 t2 = Stdlib.compare t1 t2
  end in
  let module TastMap = Map.Make (TastOrd) in

  let untype_rmloc e =
    let rmloc =
      { Ast_mapper.default_mapper with
        location = (fun _self _loc -> Location.none)
      } in
    rmloc.expr rmloc (Untypeast.untype_expression e)
  in

  (* Find identical expressions *)
  let find_identical (type a)
      (module Ord : Map.OrderedType with type t = a)
      (xs : a list) (cols : expression list)
      : bool array * expression TastMap.t =
    let module M = Map.Make (Ord) in

    let m = ref M.empty in
    let colmap = ref TastMap.empty in
    let mask = List.map2
      (fun x p -> match M.find_opt x !m with
       | None ->
          m := M.add x p !m;
          colmap := TastMap.add p p !colmap;
          true
       | Some q ->
          colmap := TastMap.add p q !colmap;
          false)
      xs cols
    in
    Array.of_list mask, !colmap
  in
  let filter ~mask xs =
    List.filteri (fun i _ -> mask.(i)) xs
  in
  let substitute_columns colmap exp =
    let rename exp =
      let super = Tast_mapper.default in
      let expr self exp =
        match TastMap.find_opt exp colmap with
        | None -> super.expr self exp
        | Some col -> col
      in
      let mapper = { super with expr } in
      mapper.expr mapper exp
    in
    rename exp
  in

  (* handle aggregate applications *)
  let sel_exp, hav_exp, ord_exps =
    if !no_aggregate then sel_exp, hav_exp, ord_exps else

    let arglist, argcols, argpats, funcs, retcols, retpats, sel_exp =
      expand_aggregate ~scopes sel_exp in

    let prj_list = ref arglist in
    let prj_cols = ref argcols in
    let prj_pats = ref argpats in

    let agg_funcs = ref funcs in
    let agg_list = ref argcols in
    let agg_cols = ref retcols in
    let agg_pats = ref retpats in

    let aux exp =
      let arglist, argcols, argpats, funcs, retcols, retpats, exp =
        expand_aggregate ~scopes exp in
      prj_list := !prj_list @ arglist;
      prj_cols := !prj_cols @ argcols;
      prj_pats := !prj_pats @ argpats;
      agg_funcs := !agg_funcs @ funcs;
      agg_list := !agg_list @ argcols;
      agg_cols := !agg_cols @ retcols;
      agg_pats := !agg_pats @ retpats;
      exp
    in
    let hav_exp =
      Option.map aux
        (match se.se_having, hav_exp with
         | None, _ -> None
         | Some sexp, None -> Some (Typecore.type_exp env sexp)
         | Some _, Some exp -> Some exp)
    in
    let ord_exps =
      List.map aux
        (match se.se_orderby, ord_exps with
         | [], _ -> []
         | sexps, [] ->
            List.map (fun (sexp, _) -> Typecore.type_exp env sexp) sexps
         | _, exps -> exps)
    in

    (* handle GROUP BY *)
    let grp_exp =
      Option.map
        (fun sexp ->
          let exp = Typecore.type_exp env sexp in
          check_no_aggregate ~scopes env exp;
          let list, cols, pats, exp = expand_product exp in
          prj_list := !prj_list @ list;
          prj_cols := !prj_cols @ cols;
          prj_pats := !prj_pats @ pats;
          exp)
        se.se_groupby
    in

    (* merge identical columns *)
    let mask, colmap =
      let xs = List.map untype_rmloc !prj_list in
      find_identical (module AstOrd) xs !prj_cols
    in
    prj_list := filter ~mask !prj_list;
    prj_cols := filter ~mask !prj_cols;
    prj_pats := filter ~mask !prj_pats;

    let grp_exp = Option.map (substitute_columns colmap) grp_exp in
    agg_list := List.map (substitute_columns colmap) !agg_list;
    agg_funcs := List.map (Option.map (substitute_columns colmap)) !agg_funcs;

    (* build an auxiliary project before grouping *)
    child :=
      { plan_loc = loc;
        plan_desc = Tplan_project (!child, !prj_list);
        plan_env = env;
        plan_cardinality = !child.plan_cardinality;
        plan_patterns = !prj_pats;
      };

    (* build an aggregate *)
    let module Ord = struct
      type t = Parsetree.expression option * Parsetree.expression
      let compare = Stdlib.compare
    end in
    let mask, colmap =
      let xs = List.map2
        (fun f e -> Option.map untype_rmloc f, untype_rmloc e)
        !agg_funcs !agg_list in
      find_identical (module Ord) xs !agg_cols
    in
    agg_funcs := filter ~mask !agg_funcs;
    agg_list := filter ~mask !agg_list;
    agg_pats := filter ~mask !agg_pats;
    let sel_exp = substitute_columns colmap sel_exp in
    let hav_exp = Option.map (substitute_columns colmap) hav_exp in
    let ord_exps = List.map (substitute_columns colmap) ord_exps in

    begin match grp_exp with
    | None ->
        child :=
          { plan_loc = loc;
            plan_desc =
              Tplan_aggregate_all (!child, !agg_funcs, !agg_list);
            plan_env = env;
            plan_cardinality = One;
            plan_patterns = !agg_pats;
          }
    | Some grp_exp ->
        let plan_loc = grp_exp.exp_loc in
        child :=
          { plan_loc;
            plan_desc =
              Tplan_aggregate (!child, grp_exp, !agg_funcs, !agg_list);
            plan_env = env;
            plan_cardinality = !child.plan_cardinality;
            plan_patterns = !agg_pats;
          }
    end;
    (sel_exp, hav_exp, ord_exps)
  in

  (* handle HAVING and ORDER BY *)
  let sel_exp =
    if !no_extra_project then sel_exp else

    let sel_list, sel_cols, sel_pats, sel_exp =
      expand_product sel_exp in

    let prj_list = ref sel_list in
    let prj_cols = ref sel_cols in
    let prj_pats = ref sel_pats in

    let aux = fun exp ->
      let list, cols, pats, exp = expand_product exp in
      prj_list := !prj_list @ list;
      prj_cols := !prj_cols @ cols;
      prj_pats := !prj_pats @ pats;
      exp
    in
    let hav_exp = Option.map aux hav_exp in
    let ord_exps = List.map aux ord_exps in

    (* merge identical columns *)
    let mask, colmap =
      let xs = List.map untype_rmloc !prj_list in
      find_identical (module AstOrd) xs !prj_cols
    in
    prj_list := filter ~mask !prj_list;
    prj_pats := filter ~mask !prj_pats;
    let sel_exp = substitute_columns colmap sel_exp in
    let hav_exp = Option.map (substitute_columns colmap) hav_exp in
    let ord_exps = List.map (substitute_columns colmap) ord_exps in

    (* build an auxiliary project *)
    child :=
      { plan_loc = loc;
        plan_desc = Tplan_project (!child, !prj_list);
        plan_env = env;
        plan_cardinality = !child.plan_cardinality;
        plan_patterns = !prj_pats;
      };
    Option.iter (fun exp ->
      child :=
        { plan_loc = exp.exp_loc;
          plan_desc = Tplan_filter (!child, exp);
          plan_env = env;
          plan_cardinality =
            (match !child.plan_cardinality with
            | Zero | One -> Zero | Many -> Many);
          plan_patterns = !prj_pats;
        })
      hav_exp;
    begin match !child.plan_cardinality with
    | Zero | One -> ()
    | Many ->
        if List.length ord_exps > 0 then
          child :=
            { plan_loc = se.se_orderby_loc;
              plan_desc = Tplan_sort (!child, ord_exps, ord_dirs);
              plan_env = env;
              plan_cardinality = !child.plan_cardinality;
              plan_patterns = !child.plan_patterns;
            }
    end;
    sel_exp
  in
  (* build the final project *)
  child :=
    { plan_loc = sel_exp.exp_loc;
      plan_desc = Tplan_project (!child, [sel_exp]);
      plan_env = env;
      plan_cardinality = !child.plan_cardinality;
      plan_patterns = [];
    };
  if se.se_distinct.txt then
    child :=
      { plan_loc = se.se_distinct.loc;
        plan_desc = Tplan_unique !child;
        plan_env = env;
        plan_cardinality = !child.plan_cardinality;
        plan_patterns = [];
      };
  end_se_scope ();
  !child

let type_aggregate env sfunct sarg =
  let lid = Longident.(Ldot (Lident "Stdlib", "agg")) in
  let path, decl = Env.lookup_type ~loc:sfunct.pexp_loc lid env in
  let vars = Ctype.instance_list decl.type_params in
  let ty_arg, ty_ret =
    match vars with
    | [arg; ret] -> arg, ret
    | _ -> failwith "type_aggregate"
  in
  let ty_funct = newconstr path vars in
  let funct = Typecore.type_expect env sfunct
      (Typecore.mk_expected ty_funct) in
  let arg = Typecore.type_expect env sarg
      (Typecore.mk_expected ty_arg) in
  funct, arg, ty_ret

let transl env plan =
  let open Untypeast in
  let open Location in
  let loc = plan.plan_loc in

  let lident ?(loc=loc) txt =
    let strs = String.split_on_char '.' (String.trim txt) in
    let lid = match Longident.unflatten strs with
      | None -> failwith "transl"
      | Some lid -> lid
    in
    mkloc lid loc
  in
  let lunit = lident "()" in

  let punit = Pat.construct lunit None in
  let pvar s = Pat.var (mkloc s loc) in
  let pint n = Pat.constant (Const.int n) in
  let ptup = function
    | [] -> punit
    | [p] -> p
    | pats -> Pat.tuple pats
  in
  let pconstr lid params =
    match params with
    | [] -> Pat.construct lid None
    | ps -> Pat.construct lid (Some ([], ptup ps))
  in

  let eunit = Exp.construct lunit None in
  let eid txt = Exp.ident (lident ~loc txt) in
  let eint n = Exp.constant (Const.int n) in
  let etup = function
    | [] -> eunit
    | [e] -> e
    | exps -> Exp.tuple exps
  in

  let fun_ pats exp =
    List.fold_right
      (fun pat exp -> Exp.fun_ ~loc Nolabel None pat exp)
      pats exp
  in
  let ($) func arg = Exp.apply func [Nolabel, arg] in

  let rec exp_of_pat pat =
    let loc = pat.Parsetree.ppat_loc in
    match pat.Parsetree.ppat_desc with
    | Ppat_var s -> Exp.ident ~loc (mkloc (Lident s.txt) loc)
    | Ppat_construct ({txt=Lident "()"; _}, None) -> eunit
    | Ppat_tuple l -> etup (List.map exp_of_pat l)
    | _ -> assert false
  in

  let (||>) a b = b $ a in
  let cmp = eid "Stdlib.compare" in
  let firstrow = eid "Stdlib.firstrow" in
  let cagg = lident "Agg" in

  let input = eid "SelectML.input" in
  let output = eid "SelectML.output" in
  let one = eid "SelectML.one" in
  let singleton = eid "SelectML.singleton" in
  let product = eid "SelectML.product" in
  let map = eid "SelectML.map" in
  let filter = eid "SelectML.filter" in
  let sort = eid "SelectML.sort" in
  let unique = eid "SelectML.unique" in
  let group_all = eid "SelectML.group_all" in
  let group = eid "SelectML.group" in

  let mkagg pat fs es =
    let accpat = ptup
      (List.mapi (fun i _ -> pvar ("__tmp_acc" ^ string_of_int i)) fs) in
    let accexps =
      List.mapi (fun i _ -> eid ("__tmp_acc" ^ string_of_int i)) fs in
    let exps = List.combine accexps es in
    let vbs, fs = List.mapi
      (fun i f ->
        let n = string_of_int i in
        let acc = "__tmp_accum" ^ n in
        let iter = "__tmp_iter" ^ n in
        let res = "__tmp_result" ^ n in
        match f with
        | None ->
            Vb.mk (pconstr cagg [pvar acc; pvar iter; pvar res]) firstrow,
            (eid acc, eid iter, eid res)
        | Some f ->
            Vb.mk (pconstr cagg [pvar acc; pvar iter; pvar res]) f,
            (eid acc, eid iter, eid res))
      fs
      |> List.split
    in
    let accum = etup (List.map (fun (acc, _, _) -> acc) fs) in
    let iter = fun_ [accpat; pat] @@ etup
      (List.map2 (fun (_, iter, _) (acc, e) -> iter $ acc $ e) fs exps)
    in
    let res = fun_ [accpat] @@ etup
        (List.map2 (fun (_, _, res) acc -> res $ acc) fs accexps)
    in
    List.fold_right
      (fun vb exp -> Exp.let_ Nonrecursive [vb] exp)
      vbs
      (Exp.construct cagg @@ Some (etup [accum; iter; res]))
  in

  (* check SelectML module *)
  let () =
    let check_exp =
      eid "ignore" $ Exp.constraint_
        (Exp.pack (Mod.ident (lident "SelectML")))
        (Typ.package (lident "Stdlib.SelectMLType") [])
    in
    ignore (Typecore.type_exp env check_exp)
  in

  let rec aux plan =
    match plan.plan_desc with
    | Tplan_null -> singleton $ eunit
    | Tplan_source e -> input $ untype_expression e

    | Tplan_product (pl1, pl2) ->
        let pat1 = ptup pl1.plan_patterns in
        let pat2 = ptup pl2.plan_patterns in
        let exp = etup @@
          List.map exp_of_pat (pl1.plan_patterns @ pl2.plan_patterns)
        in
        product $ fun_ [pat1; pat2] exp $ aux pl1 $ aux pl2

    | Tplan_project (pl, es) ->
        let pat = ptup pl.plan_patterns in
        let exp = etup (List.map untype_expression es) in
        aux pl ||> (map $ fun_ [pat] exp)

    | Tplan_filter (pl, e) ->
        let pat = ptup pl.plan_patterns in
        let exp = untype_expression e in
        aux pl ||> (filter $ fun_ [pat] exp)

    | Tplan_sort (pl, es, os) ->
        let pat = ptup pl.plan_patterns in
        let exp = etup (List.map untype_expression es) in
        let cmpfunc =
          let args = [pvar "__tmp_key"; pvar "__tmp_a"; pvar "__tmp_b"] in
          let body =
            let p1 = ptup
              (List.mapi (fun i _ -> pvar ("__tmp_a" ^ string_of_int i)) os) in
            let p2 = ptup
              (List.mapi (fun i _ -> pvar ("__tmp_b" ^ string_of_int i)) os) in
            let rec loop i os =
              let aux o =
                let a = eid ("__tmp_a" ^ string_of_int i) in
                let b = eid ("__tmp_b" ^ string_of_int i) in
                match o with
                | TAscending -> cmp $ a $ b
                | TDescending -> cmp $ b $ a
                | TUsing e -> untype_expression e $ a $ b
              in
              match os with
              | [] -> eint 0
              | o :: os ->
                  Exp.match_ (aux o)
                  [Exp.case (pint 0) (loop (i+1) os);
                   Exp.case (pvar "__tmp_res") (eid "__tmp_res")]
            in
            Exp.let_ Nonrecursive
            [Vb.mk p1 (eid "__tmp_key" $ eid "__tmp_a");
             Vb.mk p2 (eid "__tmp_key" $ eid "__tmp_b")]
            (loop 0 os)
          in
          fun_ args body
        in
        let func =
          Exp.let_ Nonrecursive
          [Vb.mk (pvar "__tmp_key") (fun_ [pat] exp);
           Vb.mk (pvar "__tmp_cmp") cmpfunc]
          (sort $ (eid "__tmp_cmp" $ eid "__tmp_key"))
        in
        aux pl ||> func

    | Tplan_aggregate_all (pl, fs, es) ->
        let pat = ptup pl.plan_patterns in
        let fs = List.map (Option.map untype_expression) fs in
        let es = List.map untype_expression es in
        aux pl ||> (group_all $ mkagg pat fs es) ||> singleton

    | Tplan_aggregate (pl, e, fs, es) ->
        let pat = ptup pl.plan_patterns in
        let exp = untype_expression e in
        let fs = List.map (Option.map untype_expression) fs in
        let es = List.map untype_expression es in
        let key = fun_ [pat] exp in
        aux pl ||> (group $ key $ mkagg pat fs es)

    | Tplan_unique pl -> aux pl ||> unique
  in
  let ast = aux plan in
  match plan.plan_cardinality with
  | One -> one $ ast
  | Zero | Many -> output $ ast

let type_select ~loc env se ty_expected_explained =
  let plan = build_plan ~loc env se in
  let transl pl =
    Typecore.type_expect env (transl env pl) ty_expected_explained in
  plan, transl

let () =
  Typecore.type_select := type_select;
  Typecore.type_aggregate := type_aggregate
