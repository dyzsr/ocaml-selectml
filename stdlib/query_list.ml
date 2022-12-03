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

(* Computations *)

type 'a t = 'a list

let one = function [x] -> x | _ -> assert false
let singleton x = [x]

let product f xs ys =
  List.concat_map (fun x -> List.map (fun y -> f x y) ys) xs

let join f xs ys =
  List.concat_map (fun x -> List.filter_map (fun y -> f x y) ys) xs

let equijoin f xs key_x ys key_y =
  let tbl = Hashtbl.create (List.length xs) in
  List.iter (fun x -> Hashtbl.add tbl (key_x x) x) xs;
  List.fold_left
    (fun acc y ->
      List.fold_left (fun acc x -> f x y :: acc)
        acc (Hashtbl.find_all tbl (key_y y)))
    [] ys

let map = List.map

let filter = List.filter

let sort = List.stable_sort

let unique l = List.sort_uniq compare l

let group_all aggf l =
  let Agg (init, update, final) = aggf in
  final (List.fold_left update init l)

let group key aggf l =
  let cmp f a b = compare (f a) (f b) in
  let Agg (init, update, final) = aggf in
  l |> List.stable_sort (cmp key)
    |> (function
       | [] -> []
       | hd :: tl ->
          let _, l, acc = List.fold_left
            (fun (prev, lst, acc) row ->
              if prev = key row then (prev, lst, update acc row)
              else (key row, final acc :: lst, update init row))
            (key hd, [], update init hd)
            tl
          in final acc :: l)

(* Input & Output *)

type 'a src = 'a list

let input s = s

let output s = s
