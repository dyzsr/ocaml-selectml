(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                                Dong Yan                                *)
(*                                                                        *)
(*   Copyright 2022 Dong Yan                                              *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typing for query plans *)

val type_select:
  loc:Location.t -> Env.t -> Parsetree.select_expr -> Typecore.type_expected ->
  Typedtree.plan * (Typedtree.plan -> Typedtree.expression)

val type_aggregate:
  Env.t -> Parsetree.expression -> Parsetree.expression ->
  Typedtree.expression * Typedtree.expression * Types.type_expr
