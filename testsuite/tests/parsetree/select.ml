SELECT 1;;

SELECT 1 FROM x;;

SELECT a, b FROM x, y;;
SELECT a * b + c * d FROM t1 <- f x, M.y;;
SELECT a, b FROM a <- x, b <- y;;
SELECT a, b FROM a <- (x, y);;

SELECT a, b FROM x, y WHERE a < b;;
SELECT a, b FROM x, y WHERE a < b && b > 2;;

SELECT a, b FROM x, y WHERE a < b && b > 2
GROUP BY a, b HAVING a < 1 ORDER BY a+b;;

SELECT (SELECT a);;
SELECT a FROM a <- (SELECT b FROM b);;

SELECT a FROM t ORDER BY a ASC, b DESC;;
SELECT a FROM t ORDER BY a ASC, b USING f, c, d DESC;;

let f a b =
  SELECT a, b, a+b
  FROM a, b
  WHERE a < b
  ORDER BY a+b, a
;;

SELECT {count x} FROM t;;
SELECT x, {sum y} FROM t;;

SELECT x, y, {count z} FROM t
GROUP BY x, y
HAVING {count z} > 1
ORDER BY {count z};;

SELECT
  (module struct
     let v = {count x}
   end : MT)
FROM t;;

SELECT DISTINCT x, y;;
SELECT DISTINCT x, y FROM t;;

SELECT x, y FROM x <- [] JOIN y <- [] ON true;;
SELECT x, y FROM x <- [] JOIN y <- [] ON true, z;;
SELECT x, y FROM x <- [] JOIN y <- [] ON true, z <- [];;
SELECT x, y FROM x <- [] JOIN y <- [] ON true,
                 z <- [] JOIN w <- [] ON false;;
SELECT x, y FROM x <- [] JOIN y <- [] ON true
                         JOIN z <- [] ON true;;
SELECT x, y FROM x <- [] JOIN y <- [] ON true
                         JOIN z <- [] ON true
                         JOIN w <- [] ON true;;
SELECT x, y FROM w, x <- [] JOIN y <- [] ON true;;
SELECT x, y FROM x <- [] JOIN w ON true, y <- [];;

SELECT x, y FROM x <- [] JOIN y <- [] ON x = y;;
SELECT x, y FROM x <- [] JOIN y <- [] ON x = y, z;;
SELECT x, y FROM x <- [] JOIN y <- [] ON x, z, w;;
SELECT x, y FROM x <- [] JOIN y <- [] ON f x y;;
SELECT x, y FROM x <- [] JOIN y <- [] ON f x y, z;;
SELECT x, y FROM x <- [] JOIN y <- [] ON (f x y), z;;
