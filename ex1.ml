(* Exercise 2.1*)

(* (1)
 # float_of_int 3 +. 2.5 ;;
 - : float = 5.5
*)

(* (2)
 # int_of_float 0.7;;
 - : int = 0
*)

(* (3)
 # if "11" > "100" then "foo" else "bar";;
 - : string = "foo"
*)

(* (4)
 # char_of_int ((int_of_char 'A') + 20);;
 - : char 'U'
*)

(* (5)
 # int_of_string "0xff";;
 - : int = 255
*)

(* Exercise 2.2 *)

(* (1)
 # if true&&false then 2;;
 予想：if文のelseが省略されているためthenの型がunitでなければならないが、そうでない。
 結果：This expression has type int but an expression was expected of type uint
*)

(* (2)
 # 8*-2;;
 予想："*"と"-"の間に空白がないので、"*-"という演算子だとパースされるが、そのような演算子がない。
 結果：Unbound value *-
*)

(* (3)
 # int_of_string "0xfg";;
 予想： "0xfg"が正しい16進数の表記でないため整数に変換できない
 結果：Failure "int_of_string"
*)

(* (4)
 # int_of_float -0.7;;
 予想："-"を二項演算子と解釈しint_of_floatがintであると予測するが、そうでない。
 結果：This expression has type float -> int but an expression was expected of type int
*)

(* Exercise 2.3 *)

(* (1)
 元の式：not true && false
*)
not true && false;;
(*- : bool = true*)

(* (2)
 元の式：float_of_int int_of_float 5.0
*)
float_of_int (int_of_float 5.0);;
(*- : float = 5.*)

(* (3)
 元の式：sin 3.14 /. 2.0 ** 2.0 +. cos 3.14 /. 2.0 ** 2.0
*)
sin (3.14 /. 2.0) ** 2.0 +. cos (3.14 /. 2.0) ** 2.0;;
(*- : float = 1.*)

(* (4)
 元の式：sqrt 3 * 3 + 4 * 4
*)
int_of_float (sqrt (float_of_int (3 * 3 + 4 * 4)));;
(*- : int = 5*)


(* Exercise 2.4 *)
let andb b1 b2 =
  if not b1 
  then false
  else b2;;
(*
  # andb true false;;
  - : bool = false
*)

let orb b1 b2=
  if b1
  then true
  else b2;;
(*
  # orb true false;;
  - : bool = true
*)


(* Exercise 2.5 *)
(*
  a_2'    ：有効
  ____    ：有効
  Cat     ：無効
  _'_'_   ：有効
  7eleven ：無効
  'ab2_   ：無効
  _       ：let _ = 3;;のように宣言は出来るが、参照できない。
*)

(* Exercise 2.6 *)
(* (1) *)
let yen_of_dollar doll =
  floor (doll *. 111.12);;

(* (2) *)
let dollar_of_yen yen =
  floor (yen /. 111.12 *. 100.) /. 100.;;

(* (3) *)
let show_dollar_yen doll =
  string_of_float doll ^ " dollars are " ^
    string_of_float (yen_of_dollar doll) ^
    " yen."
(* (4) *)
let capitalise alphabet =
  if int_of_char alphabet >= int_of_char 'a'
    && int_of_char alphabet <= int_of_char 'z'
  then char_of_int ((int_of_char alphabet) - 32)
  else alphabet;;


(* Exercise 3.1 *)
(* (1)
let x = 1 in
 let x = 3 in
   let x = x + 2 in  (* 上の行で宣言した値が3であるx *)
     x * x           (* 上の行で宣言した値が5であるx *)

(* この式を評価すると25となる *)
*)

(* (2)
let x = 2 and y = 3 in
 (let y = x and x = y + 2 in (* yに代入されるxは上の行で宣言した値が2であるx、xに代入されるyは上の行で宣言した値が3であるy *)
    x * y) (* この式の評価結果は10 *)
   + y;; (* このyは最初の行で宣言された値が3であるy *)
  (* この式の評価結果は13 *)
*)

(* (3)
let x = 2 in
  let y = 3 in
    let y = x in (* このxは最初の行で宣言された値が2のx *)
      let z = y + 2 in (* このyは上の行で宣言された値が2のy *)
        x * y * z (* このxは最初の行で宣言された値が2のx、このyは最初の行で宣言された値が2のy、このzは最初の行で宣言された値が4のz *)
  (* この式の評価結果は16 *)
*)

(* Exercise 3.2 *)
(*
  let x = e1 and y = e2;;
  let x = e1 let y = e2;;
  上はe1, e2が評価された後にそれぞれx, yの値となるのに対し、下はe1を評価しxの値とした後に、e2を評価する。そのため、もしe2にxが含まれていたとすると両者でyの値が異なることがある。
*)

(* Exercise 3.3 *)
let geo_mean (x, y) =
  sqrt (x *. y);;

(* Exercise 3.4 *)
let prodMatVec ((a, b, c, d), (x, y)) =
  ((a *. x +. b *. y),
   (c *. x +. d *. y));;

(* Exercise 3.5 *)
(*
  float * float * float * float
  (float * float) * (float * float)
  上は(a, b, c, d)で値を取るが、下は((a, b), (c, d))で値を取る。
*)

(* Exercise 3.6 *)
(*
  let (x:int) = 5;;
  xが右辺全体にマッチし、xで束縛される。
*)

(* Exercise 3.7 *)
let rec pow (x, n) =
  if n = 0
  then 1
  else x * pow (x, (n-1));;

pow (2, 10);;

let rec even n =
  if n = 0
  then true
  else odd (n-1)
and odd n =
  if n = 0
  then false
  else even (n-1);;

let rec pow2 (x, n) =
  if n = 0
  then 1
  else (if even n
    then let t = pow2 (x, (n/2))
	 in t*t
    else let t = pow2 (x, (n/2))
	 in t*t*x);;
pow2 (2, 9);;
pow2 (2, 10);;

(* Exercise 3.8 *)
(* xのn乗を求めるには
powi x n 1
とする *)
let rec powi (x, n, it) =
  if n = 0
  then it
  else powi (x, (n-1), (it*x));;

powi (3, 4, 1);;

(* Exercise 3.9 *)
(*
let cond (b, e1, e2) : int = if b then e1 else e2;;
let rec fact n = cond ((n = 1), 1, n * fact (n-1));;

関数を呼び出す時には引数は全て評価されるが、fact nを呼び出すとcond関数の引数に含まれるfact (n-1)を呼び出すことになり無限に再帰してしまう。
*)

(* Exercise 3.10 *)
let rec fib n = (* nth Fibonacci number *)
  if n = 1 || n = 2 then 1 else fib(n - 1) + fib(n - 2);;

(*
  fib 4
  if 4 = 1 || 4 == 2 then 1 else fib(4-1) + fib(4-2)
  fib 3 + fib 2
  (if 3 = 1 || 3 == 2 then 1 else fib(3-1) + fib(3-2)) + fib 2
  fib 2 + fib 1 + fib 2
  (if 2 = 1 || 2 == 2 then 1 else fib(2-1) + fib(2-2)) + fib 1 + fib 2
  1 + fib 1 + fib 2
  1 + (if 1 = 1 || 1 == 2 then 1 else fib(1-1) + fib(1-2)) + fib 2
  1 + 1 + fib 2
  2 + fib 2
  2 + (if 2 = 1 || 2 == 2 then 1 else fib(2-1) + fib(2-2))
  2 + 1
  3
*)


(* Exercise 3.11 *)
(* (1) *)
let rec gcd m n =
  if m > n
  then gcd n m
  else if (n mod m) = 0
  then m
  else gcd (n mod m) m;;

(* (2) *)
let rec combination n m =
  if m = 0 || n == m
  then 1
  else combination (n-1) m + combination (n-1) (m-1);;

(* (3) *)
(* fibonacci数のn番目を求めるには
fib_iter n 1 1
とする *)
let rec fib_iter n prev curr =
  if n = 1 || n = 2
  then curr
  else fib_iter (n-1) curr (prev + curr);;

(* (4) *)
open String
let rec max_ascii str =
  if length str = 0
  then '\000'
  else let t = max_ascii (sub str 1 (length str - 1))
       and h = str.[0]
       in if t > h
	 then t
	 else h;;


(* Exercise 3.12 *)
(*
let rec pos n =
  neg (n-1) +. 1.0 /. (float_of_int (4 * n + 1))
and neg n =
  if n < 0 then 0.0
  else pos n -. 1.0 /. (float_of_int (4 * n + 3));;
*)


let rec atan1 n =
  if n < 0
  then 0.0
  else atan1 (n-1) +. 1.0 /. (float_of_int (4 * n + 1)) -. 1.0 /. (float_of_int (4 * n + 3));;



(* Exercise 4.1 *)
let rec integral f a b =
  if a > b
  then 0.0
  else let dx = 0.1e-3
       in (f a +. f (a +. dx)) *. dx /. 2. +. integral f (a +. dx) b;;


integral sin 0. 3.141592653;;



(* Exercise 4.2 *)
let rec pow_curry x n =
  if n = 0
  then 1
  else if even n
  then let t = pow_curry x (n/2)
       in t*t
  else let t = pow_curry x (n/2)
       in t*t*x;;

let pow n x = pow_curry x n;;
let cube = pow 3;;
let cube x = pow_curry x 3;;



(* Exercise 4.3 *)
(*
int -> int -> int -> int
最終的にintとintとintを受け取りintを返す関数。
*)
let e43_test1 x y z = x + y + z;;
e43_test1 4 8 2;;
(*
(int -> int) -> int -> int
最終的にintを受け取りintを返す関数とintを受け取りintを返す関数。
*)
let e43_test2 (f:int -> int) (x:int)  = f x;;
e43_test2 cube 3;;
(*
(int -> int -> int) -> int
最終的にintとintを受け取りintを返す関数受け取りintを返す関数。
*)
let e43_test3 (f:int->int->int) = f 4 6;;
e43_test3 (+);;

(* Exercise 4.4 *)
let curry f x y = f (x, y);;
let average (x, y) = (x +. y) /. 2.0;;
let curried_avg = curry average;;
let uncurry f (x, y) = f x y;;
let uncurried_curried_avg = uncurry curried_avg;;

(* Exercise 4.5 *)
let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x) else x;;
let fib n =
  let (fibn, _) = repeat (fun (curr, prev) -> (curr + prev, curr)) (n-1) (1, 0)
  in fibn;;

let id x = x;;
let ($) f g x = f (g x);;

(* Exercise 4.6 *)
let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f;;
(*
  関数fをn個合成した合成関数を返す。
  動作は指数ををlog nで求める関数powと類似している。
*)


let k x y = x;;
let s x y z = x z (y z);;
(* Exercise 4.7 *)
(*
  s k k 1
  k 1 (k 1)
  1
*)
let xy_y = k (s k k);;
xy_y 1 2;;

(* Exercise 4.8 *)
let double f x = f (f x);;
(*
  double double f x
  (double double f) x
  double (double f) x
  (double f) ((double f) x)
  (double f) (double f x)
  double f (f (f x))
  f (f (f (f x)))
*)


let hd (x::_) = x;;
let tl (_::rest) = rest;;
let null =
  function [] -> true
  | _ -> false;;
let rec nth n l =
  if n = 1 then hd l else nth (n - 1) (tl l);;
let rec take n l =
  if n = 0 then [] else (hd l) :: (take (n - 1) (tl l));;
let rec drop n l =
  if n = 0 then l else drop (n - 1) (tl l);;
let rec length =
  function [] -> 0
  | _ :: rest -> 1 + length rest;;
let rec append l1 l2 =
  match l1 with
    [] -> l2
  | x :: rest -> x :: (append rest l2);;
let rec map f =
  function [] -> []
  | x :: rest -> f x :: map f rest;;
let rec forall p =
  function [] -> true
  | x :: rest -> if p x then forall p rest else false;;
let rec exists p =
  function [] -> false
  | x :: rest -> (p x) or (exists p rest);;
let rec fold_right f l e =
  match l with
    [] -> e
  | x :: rest -> f x (fold_right f rest e);;
let rec fold_left f e l =
  match l with
    [] -> e
  | x :: rest -> fold_left f (f e x) rest;;
let length l = fold_right (fun x y -> x + y) (map (fun _ -> 1) l) 0

(* Exercise 5.1 *)
(*
1. [[]]
a list list

2. [[1; 3]; ["hoge"]]
リストでない
第一要素は(int*int)であるが、第二要素はstringである

3. [3] :: []
int list list

4. 2 :: [3] :: []
リストでない
第一要素はintであるが、第二要素はint listである

5. [] :: []
a list list

6. [(fun x -> x); (fun b -> not b)]
(bool -> bool) list

コンパイラに入力した結果、予想は正しかった。
*)

(* Exercise 5.2 *)
let rec sum_list l =
  if null l
  then 0
  else hd l + sum_list (tl l);;

let rec max_list l =
  if null (tl l)
  then hd l
  else if (hd l) > (hd (tl l))
  then max_list ((hd l) :: (tl (tl l)))
  else max_list ((hd (tl l)) :: (tl (tl l)));;

(*
記述はパターンマッチを利用したほうが簡潔である。
*)


(* Exercise 5.3 *)
(* 1 *)
let rec downto0 n =
  if n = 0
  then [0]
  else n :: (downto0 (n-1));;

(* 2 *)
let rec roman ((x::xs) as l) n =
  if n = 0
  then ""
  else if fst x <= n
  then snd x ^ roman l (n - fst x)
  else roman xs n;;

(* 3 *)
let concat l = fold_right (@) l [];;

(* 4 *)
let rec zip a =
  function [] -> []
  | x::xs -> match a with
    | [] -> []
    | hd::tl -> (hd, x) :: (zip tl xs);;

(* 5 *)
let rec filter p l =
  match l with
  | [] -> []
  | x :: xs -> if p x
    then x :: (filter p xs)
    else (filter p xs);;

(* 6 *)
(* (a) *)
let rec belong a s =
  match s with
  | [] -> false
  | x :: xs -> if a = x
    then true
    else belong a xs;;

(* (b) *)
let rec intersect s1 =
  function [] -> []
  | x :: xs -> if belong x s1
    then x :: (intersect s1 xs)
    else intersect s1 xs;;

(* (c) *)
let rec union s1 =
  function [] -> s1
  | x :: xs -> if belong x s1
    then union s1 xs
    else x :: (union s1 xs);;

(* (d) *)
let rec diff s1 s2 =
  match s1 with
  | [] -> []
  | x :: xs -> if belong x s2
    then diff xs s2
    else x :: (diff xs s2);;


(* Exercise 5.4 *)
let map_two f g l = map (fun x -> f (g x)) l;;

(* Exercise 5.5 *)
let forall p l = fold_right (&&) (map p l) true;;
let exist p l = fold_right (||) (map p l) false;;

(* Exercise 5.6 *)
let rec quicker l sorted =
  match l with
  | [] -> sorted
  | [x] -> x :: sorted
  | x :: xs ->
    let rec partition left right =
      function [] -> quicker left (x :: (quicker right sorted))
      | y :: ys -> if x < y
	then partition left (y :: right) ys
	else partition (y :: left) right ys
    in partition [] [] xs;;

(* Exercise 5.7 *)
let square r =
  let rec s1 x l =
    if x = 0
    then l
    else (s1 (x-1) l@(map (fun y -> (x, y)) (filter (fun y -> x*x + y*y = r * r) (downto0 x))))
  in s1 r [];;

let square r =
  let rec s1 x =
    if x <= (int_of_float (sqrt ((float_of_int r) /. 2.)))
    then []
    else (s1 (x-1)) @ (s2 x (int_of_float (sqrt (float_of_int (r - x*x)))))
  and s2 x y =
    if y = 0
    then []
    else if x*x + y*y = r
    then (x, y) :: (s2 x (y-1))
    else s2 x (y-1)
  in s1 (int_of_float (sqrt (float_of_int r)));;


(* Exercise 5.8 *)
let rec map2 f l it =
  match l with
  | [] -> it
  | x::xs -> map2 f xs (it @ [f x]);;


type figure =
  Point
| Circle of int
| Rectangle of int * int
| Square of int;;

type 'a seq = Cons of 'a * (unit -> 'a seq);;
let head (Cons (x, _)) = x;;
let tail (Cons (_, f)) = f ();;
let rec take n s =
  if n = 0 then [] else head s :: take (n - 1) (tail s);;

type loc_fig = {x : int; y : int; fig : figure};;

(* Exercise 6.1 *)
let rec overlap {x=x1;y=y1;fig=f1} {x=x2;y=y2;fig=f2} =
  match (f1, f2) with
  | (Square s, Circle r) | (Circle r, Square s) ->
    (overlap {x=x1;y=y1;fig=Rectangle (s, s)} 
       {x=x2;y=y2;fig=Circle r})
  | (Point, Point) -> x1 = x2 && y1 = y2
  | (Circle r, Point) -> (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) <= r*r
  | (Point, Circle r) -> (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) <= r*r
  | (Circle r1, Circle r2) ->
    (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1) <= (r1+r2)*(r1*r2)
  | (Rectangle (w1, h1), Rectangle (w2, h2)) -> 
    (abs (x1-x2) < (w1+w2)/2) && (abs (y1-y2) < (h1+h2)/2)
  | (Rectangle (w1, h1), Square r2) -> 
    (abs (x1-x2) < (w1+r2)/2) && (abs (y1-y2) < (h1+r2)/2)
  | (Square r1, Rectangle (w2, h2)) -> 
    (abs (x1-x2) < (r1+w2)/2) && (abs (y1-y2) < (r1+h2)/2)
  | (Square r1, Square r2) ->
    (abs (x1-x2) < (r1+r2)/2) && (abs (y1-y2) < (r1+r2)/2)
  | (Point, Rectangle (w, h)) ->
    (abs (x1-x2) < w/2) && (abs (y1-y2) < h/2)
  | (Rectangle (w, h), Point) ->
    (abs (x1-x2) < w/2) && (abs (y1-y2) < h/2)
  | (Point, Square r) ->
    (abs (x1-x2) < r/2) && (abs (y1-y2) < r/2)
  | (Square r, point) ->
    (abs (x1-x2) < r/2) && (abs (y1-y2) < r/2)
  | (Rectangle (w, h), Circle r)
  | (Circle r, Rectangle (w, h)) ->
    (overlap {x=x1+w/2;y=y1+h/2;fig=Point} {x=x2;y=y2;fig=Circle r})
    || (overlap {x=x1+w/2;y=y1-h/2;fig=Point} {x=x2;y=y2;fig=Circle r})
    || (overlap {x=x1-w/2;y=y1-h/2;fig=Point} {x=x2;y=y2;fig=Circle r})
    || (overlap {x=x1-w/2;y=y1+h/2;fig=Point} {x=x2;y=y2;fig=Circle r})
    || (overlap {x=x1;y=y1;fig=Rectangle (w+r*2, h)}
	  {x=x2;y=y2;fig=Circle r})
    || (overlap {x=x1;y=y1;fig=Rectangle (w, h+r*2)}
	  {x=x2;y=y2;fig=Circle r});;



type nat = Zero
	 | OneMoreThan of nat;;
let rec add m n =
  match m with
  | Zero -> n
  | OneMoreThan m' -> OneMoreThan (add m' n);;
type 'a option = None | Some of 'a
(* Exercise 6.2 *)
let rec nat_of_int n =
  if n = 0
  then Zero
  else OneMoreThan (nat_of_int (n-1));;

let rec int_of_nat n =
  match n with
  | Zero -> 0
  | OneMoreThan m' -> 1 + int_of_nat m';;

let rec mul m n =
  match m with
  | Zero -> Zero
  | OneMoreThan m' -> add n (mul m' n);;

let rec monus m n =
  match n with
  | Zero -> m
  | OneMoreThan n' -> 
    match m with
    | Zero -> Zero
    | OneMoreThan m' -> monus m' n';;

(* Exercise 6.3 *)
let rec minus m n =
  match n with
  | Zero -> Some m
  | OneMoreThan n' -> 
    match m with
    | Zero -> None
    | OneMoreThan m' -> minus m' n';;


type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
(* Exercise 6.4 *)
let rec comptree x n =
  if n = 0
  then Lf
  else Br (x, (comptree x (n-1)), (comptree x (n-1)));;


let rec size = 
  function Lf -> 0
  | Br (_, left, right) -> 1 + size left + size right;;
let rec depth =
  function Lf -> 0
  | Br (_, left, right) -> 1 + max (depth left) (depth right);;
let ctree = Br(1, Br(2, Br(4, Lf, Lf),
		     Br(5, Lf, Lf)),
	       Br(3, Br(6, Lf, Lf),
		  Br(7, Lf, Lf)));;
(* Exercise 6.5 *)
let rec preorder =
  function Lf -> []
  | Br (x, left, right) -> x :: (preorder left) @ (preorder right);;
let rec inorder =
  function Lf -> []
  | Br (x, left, right) -> (inorder left) @ (x :: inorder right);;
let rec postorder =
  function Lf -> []
  | Br (x, left, right) -> (postorder left) @ (postorder right) @ [x];;
let rec preord t l =
  match t with
  | Lf -> l
  | Br(x, left, right) -> x :: (preord left (preord right l));;
let rec inord t l =
  match t with
  | Lf -> l
  | Br(x, left, right) -> (inord left (x::inord right l));;
let rec postord t l =
  match t with
  | Lf -> l
  | Br(x, left, right) -> (postord left (postord right (x::l)));;

(* Exercise 6.6 *)
let rec reflect =
  function Lf -> Lf
  | Br (x, left, right) -> Br (x, reflect right, reflect left);;
(*
  preorder(reflect(t)) = reverse(postorder(t))
  inorder(reflect(t)) = reverse(inorder(t))
  postorder(reflect(t)) = reverse(preorder(t))
*)


type arith =
  Const of int | Add of arith * arith | Mul of arith * arith;;
let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;
(* Exercise 6.7 *)
let rec string_of_arith =
  function Const n -> string_of_int n
  | Add (a1, a2) -> string_of_arith a1 ^ "+" ^ string_of_arith a2
  | Mul (Const n, ((Add (_, _)) as add))
    -> string_of_int n ^ "*(" ^ string_of_arith add ^ ")"
  | Mul (((Add (_, _)) as add), Const n)
    -> "(" ^ string_of_arith add ^ ")*" ^ string_of_int n
  | Mul (((Add (_, _)) as add1), ((Add (_, _)) as add2))
    -> "(" ^ string_of_arith add1 ^ ")*(" ^ string_of_arith add2 ^ ")"
  | Mul (a1, a2) -> string_of_arith a1 ^ "*" ^ string_of_arith a2;;

let rec expand =
  function 
  | Mul (Const n, Add (a1, a2))
    -> Add (expand (Mul (Const n, expand a1)),
	    expand (Mul (Const n, expand a2)))
  | Mul (Add (a1, a2), Const n)
    -> Add (expand (Mul (expand a1, Const n)),
	    expand (Mul (expand a2, Const n)))
  | Mul (Add (a1, a2), Add (a3, a4))
    -> Add (expand (Mul (expand a1, expand a3)),
	    Add (expand (Mul (expand a1, expand a4)),
		 Add (expand (Mul (expand a2, expand a3)),
		      expand (Mul (expand a2, expand a4)))))
  | _ as hoge -> hoge;;


let rec mem t x =
  match t with
    Lf -> false
  | Br (y, left, right) ->
    if x = y then true
    else if x < y then mem left x else mem right x
let rec add t x =
  match t with
    Lf -> Br (x, Lf, Lf)
  | (Br (y, left, right) as whole) ->
    if x = y then whole
    else if x < y then Br(y, add left x, right) else Br(y, left, add right x);;

(* Exercise 6.8 *)
let rec addList t =
  function [] -> t
  | x :: xs -> addList (add t x) xs;;

(*
Br (1, Lf, Br (2, Lf, Br (3, Lf, Br (4, Lf, Lf))))
1 2 3 4の順番にadd
*)

(*
Br (1, Lf, Br (2, Lf, Br (4, Br (3, Lf, Lf), Lf)))
1 2 4 3の順番にadd
*)

(*
Br (1, Lf, Br (3, Br (2, Lf, Lf), Br (4, Lf, Lf)))
1 3 2 4の順番にadd
1 3 4 2の順番にadd
*)

(*
Br (1, Lf, Br (4, Br (2, Lf, Br (3, Lf, Lf)), Lf))
1 4 2 3の順番にadd
*)

(*
Br (1, Lf, Br (4, Br (3, Br (2, Lf, Lf), Lf), Lf))
1 4 3 2の順番にadd
*)

(*
Br (2, Br (1, Lf, Lf), Br (3, Lf, Br (4, Lf, Lf)))
2 1 3 4の順番にadd
2 3 1 4の順番にadd
2 3 4 1の順番にadd
*)

(*
Br (2, Br (1, Lf, Lf), Br (4, Br (3, Lf, Lf), Lf))
2 1 4 3の順番にadd
2 4 1 3の順番にadd
2 4 3 1の順番にadd
*)

(*
Br (3, Br (1, Lf, Br (2, Lf, Lf)), Br (4, Lf, Lf))
3 1 2 4の順番にadd
3 1 4 2の順番にadd
3 4 1 2の順番にadd
*)

(*
Br (3, Br (2, Br (1, Lf, Lf), Lf), Br (4, Lf, Lf))
3 2 1 4の順番にadd
3 2 4 1の順番にadd
3 4 2 1の順番にadd
*)

(*
Br (4, Br (1, Lf, Br (2, Lf, Br (3, Lf, Lf))), Lf)
4 1 2 3の順番にadd
*)

(*
Br (4, Br (1, Lf, Br (3, Br (2, Lf, Lf), Lf)), Lf)
4 1 3 2の順番にadd
*)

(*
Br (4, Br (2, Br (1, Lf, Lf), Br (3, Lf, Lf)), Lf)
4 2 1 3の順番にadd
4 2 3 1の順番にadd
*)

(*
Br (4, Br (3, Br (1, Lf, Br (2, Lf, Lf)), Lf), Lf)
4 3 1 2の順番にadd
*)

(*
Br (4, Br (3, Br (2, Br (1, Lf, Lf), Lf), Lf), Lf)
4 3 2 1の順番にadd
*)


(* Exercise 6.9 *)
let rec from n = Cons (n, fun () -> from (n + 1));;
let rec mapseq f (Cons (x, tail)) =
  Cons (f x, fun () -> mapseq f (tail ()));;
let rec filterseq f (Cons (x, tail)) =
  if f x
  then filterseq f (tail ())
  else Cons (x, fun () -> filterseq f (tail ()));;
let rec nthseq n (Cons (x, f)) =
  if n = 1 then x else nthseq (n - 1) (f());;
let sift n = filterseq (fun x -> x mod n == 0);;
let rec sieve (Cons (x, f)) =
  Cons (x, fun () -> sieve (sift x (f())));;
let primes = sieve (from 2);;

(* nthseq (9488+3000) primes;; *)


(* Exercise 6.10 *)
type ('a, 'b) sum = Left of 'a | Right of 'b;;
(* (1) *)
let e610_1 (a, s) =
  match s with
  | Left l -> Left (a, l)
  | Right r -> Right (a, r);;
(* (2) *)
let e610_2 =
  function
  | (Left a, Left c) -> Left (Left (a, c))
  | (Left a, Right d) -> Right(Left (a, d))
  | (Right b, Left c) -> Right (Right (b, c))
  | (Right b, Right d) -> Left (Right (b, d));;
(* (3) *)
let e610_3 (fl, fr) =
  function
  | Left l -> fl l
  | Right r -> fr r;;
(* (4) *)
let e610_4 f =
  ((fun x -> f (Left x)), (fun x -> f (Right x)));;
(* (5) *)
let e610_5 =
  function
  | Left fl -> (fun x -> Left (fl x))
  | Right fr -> (fun x -> Right (fr x));;  



(* Exercise 7.1 *)
let ref x = {contents=x};;
let (!) {contents=x} = x;;
let (:=) x y = x.contents<-y;;

(* Exercise 7.2 *)
let incr x = x.contents<-(!x+1);;

(* Exercise 7.3 *)
let f = ref (fun y -> y + 1);;
let funny_fact x =
  if x = 0
  then 1
  else x * (!f (x-1));;
f := funny_fact;;
(*
funny_fact内で呼び出される関数の参照fをfunny_fact自身に書き換えることで再帰しているのと同様に動作する。
*)


(* Exercise 7.4 *)
let fact_imp n =
  let i = ref n and res = ref 1 in
  while (!i <> 0) do
    res := !res * !i;
    i := !i - 1
  done;
  !res;;

(* Exercise 7.5 *)
let rec fact n =
  if n < 0
  then raise (Invalid_argument "fact needs natural number.")
  else if n = 0
  then 1
  else n * fact (n-1);;

(* Exercise 7.6 *)
(*
let x = ref [];;
とした場合、型はテキストの様に 'a list ref ではなく、実際は '_a list ref となる。
'_aは型を決めることができると、'_aからこの型となる。
今回の例ではxにboolの値をconsした段階で '_a list ref から bool list ref になり、int をconsしようとするとエラーを検出できる。
*)

(* Exercise 7.7 *)
type pointI = {get: unit -> int; set: int -> unit; inc: unit->unit};;
let p =
  let x = ref 0 in
  let rec this () =
    {get= (fun () -> !x);
     set= (fun newx -> x:=newx);
     inc= (fun () -> (this ()).set ((this ()).get () + 1))} in
  this ();;
let pointC x =
  let rec this () =
    {get= (fun () -> !x);
     set= (fun newx -> x:=newx);
     inc= (fun () -> (this ()).set ((this ()).get () + 1))} in
  this ();;
let new_point x = pointC (ref x);;
type color = Blue | Red | Green | White;;
type cpointI = {cget: unit -> int;
		cset: int -> unit;
		cinc: unit->unit;
		getcolor: unit-> color};;
let cpointC x col=
  let super = pointC x in
  let rec this =
    {cget= super.get;
     cset= (fun x -> super.set x; col := White);
     cinc= (fun () -> super.inc (); col := White);
     getcolor = (fun () -> !col)} in
  this;;
let new_cpoint x col = cpointC (ref x) (ref col);;
(*
cincでは点の座標を増やしているだけで色を変えてないので変わるわけがない。
*)

(* Exercise 7.8 *)
let us_coins = [25; 10; 5; 1]
and gb_coins = [50; 20; 10; 5; 2; 1]
and jp_coins = [500; 100; 50; 10; 5; 1]
and hoge_coins = [5; 2];;
let rec change =
  function
  | (_, 0) -> []
  | ((x::xs) as coins, amount)
    -> (try (if x <= amount
      then x :: (change (coins, amount - x))
      else change (xs, amount)) with
	   | Match_failure _-> change (xs, amount));;
(*
パターンマッチできない状況になると例外によって一つ戻り、最大のコインを使わない場合を計算する。
*)

(* Exercise 7.9 *)
let print_int l = output_string stdout (string_of_int l);;

(* Exercise 7.10 *)
let cp src dst =
  let s = open_in src
  and d = open_out dst
  in try (while (true) do output_string d ((input_line s)^"\n"); done;) with
  | End_of_file -> print_string ("copied "^src^" to "^dst);
    close_in s;
    close_out d;;
