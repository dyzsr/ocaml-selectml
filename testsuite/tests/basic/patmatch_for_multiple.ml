(* TEST
   flags = "-drawlambda -dlambda"
   * expect
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/286 = 3 *match*/287 = 2 *match*/288 = 1)
  (catch
    (catch
      (catch (if (!= *match*/287 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/286 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/286 = 3 *match*/287 = 2 *match*/288 = 1)
  (catch (if (!= *match*/287 3) (if (!= *match*/286 1) 0 (exit 1)) (exit 1))
   with (1) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/291 = 3 *match*/292 = 2 *match*/293 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/292 3) (exit 6)
          (let (x/295 =a (makeblock 0 *match*/291 *match*/292 *match*/293))
            (exit 4 x/295)))
       with (6)
        (if (!= *match*/291 1) (exit 5)
          (let (x/294 =a (makeblock 0 *match*/291 *match*/292 *match*/293))
            (exit 4 x/294))))
     with (5) 0)
   with (4 x/289) (seq (ignore x/289) 1)))
(let (*match*/291 = 3 *match*/292 = 2 *match*/293 = 1)
  (catch
    (if (!= *match*/292 3)
      (if (!= *match*/291 1) 0
        (exit 4 (makeblock 0 *match*/291 *match*/292 *match*/293)))
      (exit 4 (makeblock 0 *match*/291 *match*/292 *match*/293)))
   with (4 x/289) (seq (ignore x/289) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/296[int] b/297 : int 0)
(function a/296[int] b/297 : int 0)
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/300[int] b/301 (let (p/302 =a (makeblock 0 a/300 b/301)) p/302))
(function a/300[int] b/301 (makeblock 0 a/300 b/301))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/304[int] b/305 (let (p/306 =a (makeblock 0 a/304 b/305)) p/306))
(function a/304[int] b/305 (makeblock 0 a/304 b/305))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/310[int] b/311
  (let (x/312 =a[int] a/310 p/313 =a (makeblock 0 a/310 b/311))
    (makeblock 0 (int,*) x/312 p/313)))
(function a/310[int] b/311
  (makeblock 0 (int,*) a/310 (makeblock 0 a/310 b/311)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/316[int] b/317
  (let (x/318 =a[int] a/316 p/319 =a (makeblock 0 a/316 b/317))
    (makeblock 0 (int,*) x/318 p/319)))
(function a/316[int] b/317
  (makeblock 0 (int,*) a/316 (makeblock 0 a/316 b/317)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/326[int] b/327[int]
  (if a/326
    (let (x/328 =a[int] a/326 p/329 =a (makeblock 0 a/326 b/327))
      (makeblock 0 (int,*) x/328 p/329))
    (let (x/330 =a b/327 p/331 =a (makeblock 0 a/326 b/327))
      (makeblock 0 (int,*) x/330 p/331))))
(function a/326[int] b/327[int]
  (if a/326 (makeblock 0 (int,*) a/326 (makeblock 0 a/326 b/327))
    (makeblock 0 (int,*) b/327 (makeblock 0 a/326 b/327))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/332[int] b/333[int]
  (catch
    (if a/332
      (let (x/340 =a[int] a/332 p/341 =a (makeblock 0 a/332 b/333))
        (exit 10 x/340 p/341))
      (let (x/338 =a b/333 p/339 =a (makeblock 0 a/332 b/333))
        (exit 10 x/338 p/339)))
   with (10 x/334[int] p/335) (makeblock 0 (int,*) x/334 p/335)))
(function a/332[int] b/333[int]
  (catch
    (if a/332 (exit 10 a/332 (makeblock 0 a/332 b/333))
      (exit 10 b/333 (makeblock 0 a/332 b/333)))
   with (10 x/334[int] p/335) (makeblock 0 (int,*) x/334 p/335)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/342[int] b/343[int]
  (if a/342
    (let (x/344 =a[int] a/342 _p/345 =a (makeblock 0 a/342 b/343))
      (makeblock 0 (int,*) x/344 [0: 1 1]))
    (let (x/346 =a[int] a/342 p/347 =a (makeblock 0 a/342 b/343))
      (makeblock 0 (int,*) x/346 p/347))))
(function a/342[int] b/343[int]
  (if a/342 (makeblock 0 (int,*) a/342 [0: 1 1])
    (makeblock 0 (int,*) a/342 (makeblock 0 a/342 b/343))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/348[int] b/349
  (let (x/350 =a[int] a/348 p/351 =a (makeblock 0 a/348 b/349))
    (makeblock 0 (int,*) x/350 p/351)))
(function a/348[int] b/349
  (makeblock 0 (int,*) a/348 (makeblock 0 a/348 b/349)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/361[int] b/362
  (catch
    (if a/361 (if b/362 (let (p/363 =a (field 0 b/362)) p/363) (exit 12))
      (exit 12))
   with (12) (let (p/364 =a (makeblock 0 a/361 b/362)) p/364)))
(function a/361[int] b/362
  (catch (if a/361 (if b/362 (field 0 b/362) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/361 b/362)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/365[int] b/366
  (catch
    (catch
      (if a/365
        (if b/366 (let (p/370 =a (field 0 b/366)) (exit 13 p/370)) (exit 14))
        (exit 14))
     with (14) (let (p/369 =a (makeblock 0 a/365 b/366)) (exit 13 p/369)))
   with (13 p/367) p/367))
(function a/365[int] b/366
  (catch
    (catch
      (if a/365 (if b/366 (exit 13 (field 0 b/366)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/365 b/366)))
   with (13 p/367) p/367))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
