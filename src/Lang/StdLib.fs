module FsLisp.Lang.StdLib

let private liftOperator operator (s: SExpr) (x: SExpr) =
    match s, x with
    | Number s, Number x -> (s, x) ||> operator
    | Number _, x -> LispError.wrongType x "number"
    | s, _ -> LispError.wrongType s "number"

let private foldNumbers operator x0 =
    List.fold (liftOperator (fun s x -> Number(operator s x))) x0

let private reduceNumbers operator =
    List.reduce (liftOperator (fun s x -> Number(operator s x)))

let private compareNumbers predicate xs =
    match xs with
    | head :: tail -> tail |> List.forall (liftOperator predicate head) |> Boolean
    | [] -> Boolean false

let private add = foldNumbers (+) (Number(Int 0))
let private sub = reduceNumbers (-)
let private mul = foldNumbers (*) (Number(Int 1))
let private div = reduceNumbers (/)
let private rem = reduceNumbers (%)

let private gt = compareNumbers (>)
let private ge = compareNumbers (>=)
let private lt = compareNumbers (<)
let private le = compareNumbers (<=)

let private cons xs =
    match xs with
    | [ x; List ys ] -> List(x :: ys)
    | [ _; expr ] -> LispError.wrongType expr "list"
    | _ -> LispError.wrongNumberOfArguments "cons" 2 xs

let private head xs =
    match xs with
    | [ List(head :: _) ] -> head
    | [ List [] ] -> Nil
    | [ expr ] -> LispError.wrongType expr "list"
    | _ -> LispError.wrongNumberOfArguments "head" 1 xs

let private tail xs =
    match xs with
    | [ List(_ :: tail) ] -> List tail
    | [ List [] ] -> List []
    | [ expr ] -> LispError.wrongType expr "list"
    | _ -> LispError.wrongNumberOfArguments "tail" 1 xs

let builtins =
    [ "+", add
      "-", sub
      "*", mul
      "/", div
      "%", rem
      ">", gt
      ">=", ge
      "<", lt
      "<=", le
      "=", List.pairwise >> List.forall (fun (x, y) -> x = y) >> Boolean
      "<>", List.pairwise >> List.exists (fun (x, y) -> x <> y) >> Boolean
      "list", List
      "cons", cons
      "head", head
      "tail", tail ]
    |> List.map (fun (s, v) -> (s, Builtin v))
    |> Map.ofList

let lambdas =
    [ "(def (not bool) (if bool false true))"
      "(def (abs x) (if (>= x 0) x (* x -1)))"
      "(def (fold f acc xs) (if (= xs ()) acc (fold f (f acc (head xs)) (tail xs))))"
      "(def (reverse xs) (fold (fn (acc x) (cons x acc)) () xs))"
      "(def (count xs) (fold (fn (acc x) (+ acc 1)) 0 xs))"
      "(def (map f xs) (reverse (fold (fn (acc x) (cons (f x) acc)) () xs)))"
      "(def (filter predicate xs) (reverse (fold (fn (acc x) (if (predicate x) (cons x acc) acc)) () xs)))"
      "(def (range start stop) (let ((loop acc x) (if (< x start) acc (loop (cons x acc) (- x 1)))) (loop () (- stop 1))))"
      "(def (concat xs ys) (let ((loop xs ys) (if (= xs (list)) ys (loop (tail xs) (cons (head xs) ys)))) (loop (reverse xs) ys)))"
      "(def (sqrt x) (let ((iter s) (if (<= (abs (- (* s s) x)) 0.0001) s (iter (/ (+ s (/ x s)) 2)))) (if (< x 0) nil (iter 1.0))))" ]
    |> List.choose (fun x ->
        match Parser.parse x with
        | Ok expr, _, _ -> Some expr
        | _ -> None)
