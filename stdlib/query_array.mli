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

type 'a src = 'a array
type 'a t = 'a array

val input : 'a src -> 'a t
val output : 'a t -> 'a src

val one : 'a t -> 'a
val singleton : 'a -> 'a t
val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val join : ('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
val join_eq : ('a -> 'b -> 'c) ->
  'a t -> ('a -> 'k) -> 'b t -> ('b -> 'k) -> 'c t
val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val sort : ('a -> 'a -> int) -> 'a t -> 'a t
val unique : 'a t -> 'a t
val group_all : ('a, 'b) agg -> 'a t -> 'b
val group : ('a -> 'c) -> ('a, 'b) agg -> 'a t -> 'b t

