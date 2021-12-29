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
  ?in_function:(Location.t * Types.type_expr) -> loc:Location.t -> Env.t ->
  Parsetree.select_expr -> Typecore.type_expected ->
  Typedtree.expression * Typedtree.plan

val type_aggregate:
  ?in_function:(Location.t * Types.type_expr) -> Env.t ->
  Parsetree.expression -> Parsetree.expression ->
  Typedtree.expression * Typedtree.expression * Types.type_expr

val transl_plan:
  ?in_function:(Location.t * Types.type_expr) -> Env.t ->
  Typedtree.plan -> Typecore.type_expected ->
  Typedtree.expression * Typedtree.plan
