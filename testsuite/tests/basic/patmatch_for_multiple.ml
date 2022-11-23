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
(let (*match*/288 = 3 *match*/289 = 2 *match*/290 = 1)
  (catch
    (catch
      (catch (if (!= *match*/289 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/288 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/288 = 3 *match*/289 = 2 *match*/290 = 1)
  (catch (if (!= *match*/289 3) (if (!= *match*/288 1) 0 (exit 1)) (exit 1))
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
(let (*match*/293 = 3 *match*/294 = 2 *match*/295 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/294 3) (exit 6)
          (let (x/297 =a (makeblock 0 *match*/293 *match*/294 *match*/295))
            (exit 4 x/297)))
       with (6)
        (if (!= *match*/293 1) (exit 5)
          (let (x/296 =a (makeblock 0 *match*/293 *match*/294 *match*/295))
            (exit 4 x/296))))
     with (5) 0)
   with (4 x/291) (seq (ignore x/291) 1)))
(let (*match*/293 = 3 *match*/294 = 2 *match*/295 = 1)
  (catch
    (if (!= *match*/294 3)
      (if (!= *match*/293 1) 0
        (exit 4 (makeblock 0 *match*/293 *match*/294 *match*/295)))
      (exit 4 (makeblock 0 *match*/293 *match*/294 *match*/295)))
   with (4 x/291) (seq (ignore x/291) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/298[int] b/299 : int 0)
(function a/298[int] b/299 : int 0)
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
(function a/302[int] b/303 (let (p/304 =a (makeblock 0 a/302 b/303)) p/304))
(function a/302[int] b/303 (makeblock 0 a/302 b/303))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/306[int] b/307 (let (p/308 =a (makeblock 0 a/306 b/307)) p/308))
(function a/306[int] b/307 (makeblock 0 a/306 b/307))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/312[int] b/313
  (let (x/314 =a[int] a/312 p/315 =a (makeblock 0 a/312 b/313))
    (makeblock 0 (int,*) x/314 p/315)))
(function a/312[int] b/313
  (makeblock 0 (int,*) a/312 (makeblock 0 a/312 b/313)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/318[int] b/319
  (let (x/320 =a[int] a/318 p/321 =a (makeblock 0 a/318 b/319))
    (makeblock 0 (int,*) x/320 p/321)))
(function a/318[int] b/319
  (makeblock 0 (int,*) a/318 (makeblock 0 a/318 b/319)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/328[int] b/329[int]
  (if a/328
    (let (x/330 =a[int] a/328 p/331 =a (makeblock 0 a/328 b/329))
      (makeblock 0 (int,*) x/330 p/331))
    (let (x/332 =a b/329 p/333 =a (makeblock 0 a/328 b/329))
      (makeblock 0 (int,*) x/332 p/333))))
(function a/328[int] b/329[int]
  (if a/328 (makeblock 0 (int,*) a/328 (makeblock 0 a/328 b/329))
    (makeblock 0 (int,*) b/329 (makeblock 0 a/328 b/329))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/334[int] b/335[int]
  (catch
    (if a/334
      (let (x/342 =a[int] a/334 p/343 =a (makeblock 0 a/334 b/335))
        (exit 10 x/342 p/343))
      (let (x/340 =a b/335 p/341 =a (makeblock 0 a/334 b/335))
        (exit 10 x/340 p/341)))
   with (10 x/336[int] p/337) (makeblock 0 (int,*) x/336 p/337)))
(function a/334[int] b/335[int]
  (catch
    (if a/334 (exit 10 a/334 (makeblock 0 a/334 b/335))
      (exit 10 b/335 (makeblock 0 a/334 b/335)))
   with (10 x/336[int] p/337) (makeblock 0 (int,*) x/336 p/337)))
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
(function a/344[int] b/345[int]
  (if a/344
    (let (x/346 =a[int] a/344 _p/347 =a (makeblock 0 a/344 b/345))
      (makeblock 0 (int,*) x/346 [0: 1 1]))
    (let (x/348 =a[int] a/344 p/349 =a (makeblock 0 a/344 b/345))
      (makeblock 0 (int,*) x/348 p/349))))
(function a/344[int] b/345[int]
  (if a/344 (makeblock 0 (int,*) a/344 [0: 1 1])
    (makeblock 0 (int,*) a/344 (makeblock 0 a/344 b/345))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/350[int] b/351
  (let (x/352 =a[int] a/350 p/353 =a (makeblock 0 a/350 b/351))
    (makeblock 0 (int,*) x/352 p/353)))
(function a/350[int] b/351
  (makeblock 0 (int,*) a/350 (makeblock 0 a/350 b/351)))
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
(function a/363[int] b/364
  (catch
    (if a/363 (if b/364 (let (p/365 =a (field 0 b/364)) p/365) (exit 12))
      (exit 12))
   with (12) (let (p/366 =a (makeblock 0 a/363 b/364)) p/366)))
(function a/363[int] b/364
  (catch (if a/363 (if b/364 (field 0 b/364) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/363 b/364)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/367[int] b/368
  (catch
    (catch
      (if a/367
        (if b/368 (let (p/372 =a (field 0 b/368)) (exit 13 p/372)) (exit 14))
        (exit 14))
     with (14) (let (p/371 =a (makeblock 0 a/367 b/368)) (exit 13 p/371)))
   with (13 p/369) p/369))
(function a/367[int] b/368
  (catch
    (catch
      (if a/367 (if b/368 (exit 13 (field 0 b/368)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/367 b/368)))
   with (13 p/369) p/369))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
