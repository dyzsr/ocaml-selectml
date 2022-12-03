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

type 'a t = 'a array

let one t = t.(0)

let singleton x = [|x|]

let product f t1 t2 =
  let n1 = Array.length t1 and n2 = Array.length t2 in
  let len = n1 * n2 in
  Array.init len (fun i -> f t1.(i / n2) t2.(i mod n2))

let join f t1 t2 =
  let s1 = Array.to_seq t1 in
  let s2 = Array.to_seq t2 in
  Array.of_seq (Seq.filter_map (fun (a, b) -> f a b) (Seq.product s1 s2))

let equijoin f t1 k1 t2 k2 =
  let n1 = Array.length t1 in
  let tbl = Hashtbl.create n1 in
  for i = 0 to n1-1 do
    Hashtbl.add tbl (k1 t1.(i)) t1.(i)
  done;
  let acc = Array.fold_left
    (fun acc y ->
      List.fold_left (fun acc x -> f x y :: acc)
        acc (Hashtbl.find_all tbl (k2 y)))
    [] t2 in
  Array.of_list acc

let map = Array.map

let filter f t =
  let len = ref 0 in
  let b = map (fun x -> if f x then (incr len; true) else false) t in
  let j = ref 0 in
  let next _ = while not (b.(!j)) do incr j done; incr j; t.(!j-1) in
  Array.init !len next

let sort key t =
  let t' = Array.copy t in
  Array.stable_sort key t'; t'

let unique t =
  let ht = Hashtbl.create (Array.length t) in
  filter (fun x -> try Hashtbl.find ht x with
    Not_found -> Hashtbl.add ht x false; true) t

let group_all f t =
  let Agg (init, update, final) = f in
  let acc = ref init in
  for i = 0 to Array.length t - 1 do acc := update !acc t.(i) done;
  final !acc

let group key f t =
  let Agg (init, update, final) = f in
  let ht = Hashtbl.create (Array.length t) in
  for i = 0 to Array.length t - 1 do
    let k = key t.(i) in
    let acc = try Hashtbl.find ht k with Not_found -> init in
    Hashtbl.replace ht k (update acc t.(i))
  done;
  Array.map final (Array.of_seq (Hashtbl.to_seq_values ht))

type 'a src = 'a array

let input t = t

let output t = t
