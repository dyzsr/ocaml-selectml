(* TEST
   * expect
*)

type ('a, 'b) t = {a: 'a; b: 'b};;
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
|}];;

SELECT 1,2;;
[%%expect {|
- : int * int = (1, 2)
|}];;

SELECT x FROM x <- [];;
[%%expect {|
- : 'a SelectML.src = []
|}];;

SELECT x FROM x <- [1;2;3];;
[%%expect {|
- : int SelectML.src = [1; 2; 3]
|}];;

SELECT x FROM (x, y) <- [(1,4);(2,5);(3,6)];;
SELECT x, y FROM (x, y) <- [(1,4);(2,5);(3,6)];;
SELECT x, y, z
FROM (x, y) <- [(1,4);(2,5);(3,6)], z <- [7;8;9];;
SELECT x + y + z
FROM (x, y) <- [(1,4);(2,5);(3,6)], z <- [7;8;9];;
[%%expect {|
- : int SelectML.src = [1; 2; 3]
- : (int * int) SelectML.src = [(1, 4); (2, 5); (3, 6)]
- : (int * int * int) SelectML.src =
[(1, 4, 7); (1, 4, 8); (1, 4, 9); (2, 5, 7); (2, 5, 8); (2, 5, 9); (3, 6, 7);
 (3, 6, 8); (3, 6, 9)]
- : int SelectML.src = [12; 13; 14; 14; 15; 16; 16; 17; 18]
|}];;

SELECT 1 WHERE true;;
SELECT 1 WHERE false;;
[%%expect {|
- : int SelectML.src = [1]
- : int SelectML.src = []
|}];;

SELECT x, y FROM x <- [1;2;3], y <- [4;5;6] ORDER BY x, y DESC;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6] ORDER BY x DESC, y;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6] ORDER BY x ASC, y DESC;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6] ORDER BY x DESC, y ASC;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6] ORDER BY x DESC, y DESC;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6] ORDER BY (x, y) DESC;;
[%%expect {|
- : (int * int) SelectML.src =
[(1, 6); (1, 5); (1, 4); (2, 6); (2, 5); (2, 4); (3, 6); (3, 5); (3, 4)]
- : (int * int) SelectML.src =
[(3, 4); (3, 5); (3, 6); (2, 4); (2, 5); (2, 6); (1, 4); (1, 5); (1, 6)]
- : (int * int) SelectML.src =
[(1, 6); (1, 5); (1, 4); (2, 6); (2, 5); (2, 4); (3, 6); (3, 5); (3, 4)]
- : (int * int) SelectML.src =
[(3, 4); (3, 5); (3, 6); (2, 4); (2, 5); (2, 6); (1, 4); (1, 5); (1, 6)]
- : (int * int) SelectML.src =
[(3, 6); (3, 5); (3, 4); (2, 6); (2, 5); (2, 4); (1, 6); (1, 5); (1, 4)]
- : (int * int) SelectML.src =
[(3, 6); (3, 5); (3, 4); (2, 6); (2, 5); (2, 4); (1, 6); (1, 5); (1, 4)]
|}];;

let odd_first a b =
  let x = a mod 2 = 0 in
  let y = b mod 2 = 0 in
  compare x y
;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6]
  ORDER BY x USING odd_first, y ASC;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6]
  ORDER BY x USING odd_first, y DESC;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6]
  ORDER BY x ASC, y USING odd_first;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6]
  ORDER BY x DESC, y USING odd_first;;
SELECT x, y FROM x <- [1;2;3], y <- [4;5;6]
  ORDER BY x USING odd_first, y USING odd_first;;
[%%expect {|
val odd_first : int -> int -> int = <fun>
- : (int * int) SelectML.src =
[(1, 4); (3, 4); (1, 5); (3, 5); (1, 6); (3, 6); (2, 4); (2, 5); (2, 6)]
- : (int * int) SelectML.src =
[(1, 6); (3, 6); (1, 5); (3, 5); (1, 4); (3, 4); (2, 6); (2, 5); (2, 4)]
- : (int * int) SelectML.src =
[(1, 5); (1, 4); (1, 6); (2, 5); (2, 4); (2, 6); (3, 5); (3, 4); (3, 6)]
- : (int * int) SelectML.src =
[(3, 5); (3, 4); (3, 6); (2, 5); (2, 4); (2, 6); (1, 5); (1, 4); (1, 6)]
- : (int * int) SelectML.src =
[(1, 5); (3, 5); (1, 4); (1, 6); (3, 4); (3, 6); (2, 5); (2, 4); (2, 6)]
|}];;

SELECT x, y+1
FROM x <- [1;2;3], y <- [4;5;6]
WHERE x + y < 8
ORDER BY x;;
[%%expect {|
- : (int * int) SelectML.src =
[(1, 5); (1, 6); (1, 7); (2, 5); (2, 6); (3, 5)]
|}];;

{count 1};;
[%%expect {|
Line 1, characters 0-9:
1 | {count 1};;
    ^^^^^^^^^
Error: Standalone aggregate is not allowed
|}];;

SELECT {count 1};;
SELECT {count 1} WHERE true;;
SELECT {count 1} WHERE false;;
SELECT {count 1} GROUP BY 1;;
[%%expect {|
- : int = 1
- : int = 1
- : int = 0
- : int = 1
|}];;

SELECT {count x} FROM x <- [1;2;3];;
[%%expect {|
- : int = 3
|}];;

SELECT x, {count y}
FROM x <- [1;2;3], y <- [4;5;6]
WHERE x+y < 8
GROUP BY x
ORDER BY {count y};;
[%%expect {|
- : (int * int) SelectML.src = [(3, 1); (2, 2); (1, 3)]
|}];;

let cnt = ref 0;;

let f = Agg (
  0,
  (fun acc _ -> cnt := !cnt + 1; acc+1),
  (fun x -> x)
) in
SELECT {f x} FROM x <- [3;3;3;2;2;1] GROUP BY x ORDER BY {f x};;

cnt;;
[%%expect {|
val cnt : int ref = {contents = 0}
- : int SelectML.src = [1; 2; 3]
- : int ref = {contents = 6}
|}];;

let t = [ {a=1; b=1}; {a=1; b=2}; {a=1; b=3}; {a=1; b=4};
          {a=2; b=2}; {a=2; b=3}; {a=2; b=4};
          {a=3; b=3}; {a=3; b=4};
          {a=4; b=4};
        ];;

SELECT {a=t.a+1; b=t.b+2} FROM t ORDER BY t.a+1, t.b+2;;

[%%expect{|
val t : (int, int) t list =
  [{a = 1; b = 1}; {a = 1; b = 2}; {a = 1; b = 3}; {a = 1; b = 4};
   {a = 2; b = 2}; {a = 2; b = 3}; {a = 2; b = 4}; {a = 3; b = 3};
   {a = 3; b = 4}; {a = 4; b = 4}]
- : (int, int) t SelectML.src =
[{a = 2; b = 3}; {a = 2; b = 4}; {a = 2; b = 5}; {a = 2; b = 6};
 {a = 3; b = 4}; {a = 3; b = 5}; {a = 3; b = 6}; {a = 4; b = 5};
 {a = 4; b = 6}; {a = 5; b = 6}]
|}];;

let cnt = ref 0;;

SELECT {a = (cnt:=!cnt+1; t.a); b = t.b}
FROM t
WHERE t.a > 1
ORDER BY (cnt:=!cnt+1; t.a)
;;

cnt;;

[%%expect{|
val cnt : int ref = {contents = 0}
- : (int, int) t SelectML.src =
[{a = 2; b = 2}; {a = 2; b = 3}; {a = 2; b = 4}; {a = 3; b = 3};
 {a = 3; b = 4}; {a = 4; b = 4}]
- : int ref = {contents = 6}
|}];;

SELECT {sum t.a}, {count t.b}
FROM t
;;
[%%expect {|
- : int * int = (20, 10)
|}];;

SELECT {sum t.a}, {count t.b}, {avg t.b}
FROM t
;;
[%%expect {|
- : int * int * int = (20, 10, 3)
|}];;

SELECT t.a, {count t.b}, {sum t.b}, {avg t.b}
FROM t
WHERE t.a + t.b > 2
GROUP BY t.a
HAVING {count t.b} > 1
ORDER BY {sum t.b}, t.a
;;
[%%expect {|
- : (int * int * int * int) SelectML.src =
[(3, 2, 7, 3); (1, 3, 9, 3); (2, 3, 9, 3)]
|}];;

SELECT (SELECT {count x}) FROM x <- t;;

[%%expect{|
- : int = 10
|}];;

SELECT {count {count t}} FROM t;;

[%%expect{|
Line 1, characters 14-23:
1 | SELECT {count {count t}} FROM t;;
                  ^^^^^^^^^
Error: Invalid use of aggregate functions.
|}];;

let pairs x y =
  SELECT x, y FROM x, y WHERE x < y ORDER BY x

let foo f =
  SELECT {f x} FROM x <- [1;2;3;1;2;1] GROUP BY x ORDER BY {f x}

[%%expect{|
val pairs : 'a SelectML.src -> 'a SelectML.src -> ('a * 'a) SelectML.src =
  <fun>
val foo : (int, 'a) agg -> 'a SelectML.src = <fun>
|}];;

SELECT DISTINCT t.a FROM t;;
SELECT DISTINCT t.b FROM t;;
SELECT t.a + t.b FROM t;;
SELECT DISTINCT t.a + t.b FROM t;;
SELECT DISTINCT t.a, t.b FROM t;;

[%%expect{|
- : int SelectML.src = [1; 2; 3; 4]
- : int SelectML.src = [1; 2; 3; 4]
- : int SelectML.src = [2; 3; 4; 5; 4; 5; 6; 6; 7; 8]
- : int SelectML.src = [2; 3; 4; 5; 6; 7; 8]
- : (int * int) SelectML.src =
[(1, 1); (1, 2); (1, 3); (1, 4); (2, 2); (2, 3); (2, 4); (3, 3); (3, 4);
 (4, 4)]
|}];;

SELECT {sum t.a} FROM t GROUP BY t.a ORDER BY {sum t.a};;
SELECT DISTINCT {sum t.a} FROM t GROUP BY t.a ORDER BY {sum t.a};;

[%%expect{|
- : int SelectML.src = [4; 4; 6; 6]
- : int SelectML.src = [4; 6]
|}];;

let module SelectML = struct
  include SelectML
  let group key aggf l =
    let cmp f a b = compare (f a) (f b) in
    let Agg (acc, iter, res) = aggf in
    l |> List.to_seq
      |> Seq.group (fun a b -> cmp key a b = 0)
      |> Seq.map (fun g -> res (Seq.fold_left iter acc g))
      |> List.of_seq
end in
SELECT x FROM x <- [1;2;3;1;2;3] GROUP BY x ORDER BY x;;

[%%expect{|
- : int list = [1; 1; 2; 2; 3; 3]
|}];;

let module SelectML = struct
  include SelectML
  let group key aggf l = ()
end in
SELECT x FROM x <- [1;2;3;1;2;3] GROUP BY x ORDER BY x;;

[%%expect{|
File "_none_", line 1:
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a src = 'a list
           type 'a t = 'a list
           val input : 'a src -> 'a t
           val output : 'a t -> 'a src
           val one : 'a t -> 'a
           val singleton : 'a -> 'a t
           val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
           val map : ('a -> 'b) -> 'a t -> 'b t
           val filter : ('a -> bool) -> 'a t -> 'a t
           val sort : ('a -> 'a -> int) -> 'a t -> 'a t
           val unique : 'a t -> 'a t
           val group_all : ('a, 'b) agg -> 'a t -> 'b
           val group : 'a -> 'b -> 'c -> unit
         end
       is not included in
         SelectMLType
       Values do not match:
         val group : 'a -> 'b -> 'c -> unit
       is not included in
         val group : ('a -> 'c) -> ('a, 'b) agg -> 'a t -> 'b t
       The type ('a -> 'b) -> ('a, 'c) agg -> 'a t -> unit
       is not compatible with the type
         ('a -> 'b) -> ('a, 'c) agg -> 'a t -> 'c t
       Type unit is not compatible with type 'c t = 'c list
       File "stdlib.mli", line 1422, characters 2-56: Expected declaration
|}];;

module SelectML = struct
  include SelectML
  type 'a src = 'a array
  let input = Array.to_list
  let output = Array.of_list
end;;

[%%expect {|
module SelectML :
  sig
    type 'a t = 'a list
    val one : 'a t -> 'a
    val singleton : 'a -> 'a t
    val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val filter : ('a -> bool) -> 'a t -> 'a t
    val sort : ('a -> 'a -> int) -> 'a t -> 'a t
    val unique : 'a t -> 'a t
    val group_all : ('a, 'b) agg -> 'a t -> 'b
    val group : ('a -> 'c) -> ('a, 'b) agg -> 'a t -> 'b t
    type 'a src = 'a array
    val input : 'a array -> 'a list
    val output : 'a list -> 'a array
  end
|}];;

SELECT 1, 2;;

SELECT x FROM x <- [|1;2;3|];;
SELECT x FROM x <- [|1;2;3|] WHERE x mod 2 = 1;;

SELECT x, y FROM x <- [|1;2;3|], y <- [|4;5;6|]
ORDER BY x USING odd_first, y USING odd_first;;

[%%expect {|
- : int * int = (1, 2)
- : int array = [|1; 2; 3|]
- : int array = [|1; 3|]
- : (int * int) array =
[|(1, 5); (3, 5); (1, 4); (1, 6); (3, 4); (3, 6); (2, 5); (2, 4); (2, 6)|]
|}];;

let t2 = Array.of_list t;;

SELECT t.a + t.b FROM t <- t2;;
SELECT DISTINCT t.a + t.b FROM t <- t2;;

SELECT {sum t.a} FROM t <- t2 GROUP BY t.a ORDER BY {sum t.a};;
SELECT DISTINCT {sum t.a} FROM t <- t2 GROUP BY t.a ORDER BY {sum t.a};;

[%%expect {|
val t2 : (int, int) t array =
  [|{a = 1; b = 1}; {a = 1; b = 2}; {a = 1; b = 3}; {a = 1; b = 4};
    {a = 2; b = 2}; {a = 2; b = 3}; {a = 2; b = 4}; {a = 3; b = 3};
    {a = 3; b = 4}; {a = 4; b = 4}|]
- : int array = [|2; 3; 4; 5; 4; 5; 6; 6; 7; 8|]
- : int array = [|2; 3; 4; 5; 6; 7; 8|]
- : int array = [|4; 4; 6; 6|]
- : int array = [|4; 6|]
|}];;

module SelectML = struct
  type 'a t = 'a array

  let one t = t.(0)
  let singleton x = [|x|]
  let product f t1 t2 =
    let n1 = Array.length t1 and n2 = Array.length t2 in
    let len = n1 * n2 in
    Array.init len (fun i -> f t1.(i / n2) t2.(i mod n2))

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

  type 'a src = 'a list
  let input = Array.of_list
  let output = Array.to_list
end;;

[%%expect {|
module SelectML :
  sig
    type 'a t = 'a array
    val one : 'a array -> 'a
    val singleton : 'a -> 'a array
    val product : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
    val map : ('a -> 'b) -> 'a array -> 'b array
    val filter : ('a -> bool) -> 'a array -> 'a array
    val sort : ('a -> 'a -> int) -> 'a array -> 'a array
    val unique : 'a array -> 'a array
    val group_all : ('a, 'b) agg -> 'a array -> 'b
    val group : ('a -> 'b) -> ('a, 'c) agg -> 'a array -> 'c array
    type 'a src = 'a list
    val input : 'a list -> 'a array
    val output : 'a array -> 'a list
  end
|}];;

SELECT 1, 2;;

SELECT x FROM x <- [1;2;3];;
SELECT x FROM x <- [1;2;3] WHERE x mod 2 = 1;;

SELECT x, y FROM x <- [1;2;3], y <- [4;5;6]
ORDER BY x USING odd_first, y USING odd_first;;

SELECT t.a + t.b FROM t;;
SELECT DISTINCT t.a + t.b FROM t;;

SELECT {sum t.a} FROM t
GROUP BY t.a ORDER BY {sum t.a};;
SELECT DISTINCT {sum t.a} FROM t
GROUP BY t.a ORDER BY {sum t.a};;

[%%expect {|
- : int * int = (1, 2)
- : int list = [1; 2; 3]
- : int list = [1; 3]
- : (int * int) list =
[(1, 5); (3, 5); (1, 4); (1, 6); (3, 4); (3, 6); (2, 5); (2, 4); (2, 6)]
- : int list = [2; 3; 4; 5; 4; 5; 6; 6; 7; 8]
- : int list = [2; 3; 4; 5; 6; 7; 8]
- : int list = [4; 4; 6; 6]
- : int list = [4; 6]
|}];;

module M (SelectML : SelectMLType) = struct
  let f xs ys = SELECT x, y FROM x <- xs, y <- ys WHERE x < y;;
end;;

[%%expect {|
module M :
  functor (SelectML : SelectMLType) ->
    sig
      val f : 'a SelectML.src -> 'a SelectML.src -> ('a * 'a) SelectML.src
    end
|}];;
