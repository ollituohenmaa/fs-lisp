namespace FsLisp

open System

exception LispError of message: string with

    static member raise message = raise (LispError message)

    static member wrongType value expectedType =
        LispError.raise $"Type mismatch: {value} is not a {expectedType}."

    static member wrongNumberOfArguments name expected got =
        LispError.raise $"Wrong number of arguments for {name}: expected {expected}, got {List.length got}."

[<CustomEquality; CustomComparison>]
type Number =
    | Int of int
    | Float of float

    static member private operator fnInt fnFloat x y =
        match x, y with
        | Int x, Int y -> fnInt x y |> Int
        | Int x, Float y -> fnFloat (float x) y |> Float
        | Float x, Int y -> fnFloat x (float y) |> Float
        | Float x, Float y -> fnFloat x y |> Float

    static member (+)(x, y) = Number.operator (+) (+) x y
    static member (-)(x, y) = Number.operator (-) (-) x y
    static member (*)(x, y) = Number.operator (*) (*) x y
    static member (/)(x, y) = Number.operator (/) (/) x y
    static member (%)(x, y) = Number.operator (%) (%) x y

    override this.ToString() =
        match this with
        | Int x -> string x
        | Float x -> string x
    
    interface IEquatable<Number> with
        member this.Equals(other) =
            match this, other with
            | Int x, Int y -> x = y
            | Int x, Float y -> (float x) = y
            | Float x, Int y -> x = (float y)
            | Float x, Float y -> x = y

    override this.Equals(other) =
        match other with
        | :? Number as other -> (this :> IEquatable<_>).Equals(other)
        | _ -> false
    
    override _.GetHashCode() =
        raise (NotImplementedException())
    
    interface IComparable<Number> with
        member this.CompareTo(other) =
            let isSmallerThanOther =
                match this, other with
                | Int x, Int y -> x < y
                | Int x, Float y -> (float x) < y
                | Float x, Int y -> x < (float y)
                | Float x, Float y -> x < y
            if isSmallerThanOther then -1 elif this = other then 0 else 1
    
    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Number as other -> (this :> IComparable<_>).CompareTo(other)
            | _ -> -1

[<CustomEquality; NoComparison>]
type SExpr =
    | Symbol of string
    | Number of Number
    | Boolean of bool
    | Builtin of (SExpr list -> SExpr)
    | Lambda of parameters: string list * body: SExpr
    | List of SExpr list
    | Nil

    override this.ToString() =
        match this with
        | Symbol s -> s
        | Number x -> string x
        | Boolean b -> if b then "true" else "false"
        | Builtin f -> "built-in"
        | Lambda (parameters, body) -> sprintf "(fn (%s) %s)" (String.Join(" ", parameters)) (string body)
        | List xs -> sprintf "(%s)" (String.Join(" ", xs))
        | Nil -> "nil"

    interface IEquatable<SExpr> with
            member this.Equals(other) =
                match this, other with
                | Builtin _, Builtin _ -> false
                | Symbol a, Symbol b -> a = b
                | Number a, Number b -> a = b
                | Boolean a, Boolean b -> a = b
                | Lambda (a1, a2), Lambda (b1, b2) -> a1 = b1 && a2 = b2
                | List a, List b -> a = b
                | Nil, Nil -> true
                | _ -> false

    override this.Equals(other) =
        match other with
        | :? SExpr as other -> (this :> IEquatable<_>).Equals(other)
        | _ -> false
    
    override _.GetHashCode() =
        raise (NotImplementedException())

type Environment(map: Map<string, SExpr>, ?parent: Environment) =

    let mutable map = map

    let exprOrdering expr =
        match expr with
        | List _ -> 0
        | Symbol _ -> 1
        | Boolean _ -> 2
        | Number _ -> 3
        | Nil -> 4
        | Lambda _ -> 5
        | Builtin _ -> 6

    member _.Add(symbol, expr) =
        map <- map.Add(symbol, expr)

    member _.Find(s) =
        match map.TryFind(s), parent with
        | Some value, _ -> value
        | None, Some parent -> parent.Find(s)
        | None, _ -> LispError.raise $"Symbol {s} is undefined."
    
    member this.CreateFunctionEnvironment(parameters: _ list, arguments: _ list) =
        Environment((parameters, arguments) ||> List.zip |> Map.ofList, this)
    
    member _.ToArray() =
        map |> Map.toArray |> Array.sortBy (fun (s, e) -> (exprOrdering e, s))

module Keyword =

    [<Literal>]
    let Definition = "def"

    [<Literal>]
    let Lambda = "fn"

    [<Literal>]
    let Conditional = "if"

    [<Literal>]
    let Quote = "quote"

module SExpr =

    let private toSymbol expr =
        match expr with
        | Symbol s -> s
        | _ -> LispError.wrongType expr "symbol"

    let private liftOperator operator (s: SExpr) (x: SExpr) =
        match s, x with
        | Number s, Number x -> (s, x) ||> operator
        | Number _, x -> LispError.wrongType x "number"
        | s, _ -> LispError.wrongType s "number"

    let private foldNumbers operator x0 = List.fold (liftOperator (fun s x -> Number (operator s x))) x0

    let private reduceNumbers operator = List.reduce (liftOperator (fun s x -> Number (operator s x)))
    
    let private compareNumbers predicate xs =
        match xs with
        | head :: tail -> tail |> List.forall (liftOperator predicate head) |> Boolean
        | [] -> Boolean false

    let add = foldNumbers (+) (Number (Int 0))
    let sub = reduceNumbers (-)
    let mul = foldNumbers (*) (Number (Int 1))
    let div = reduceNumbers (/)
    let rem = reduceNumbers (%)

    let gt = compareNumbers (>)
    let ge = compareNumbers (>=)
    let lt = compareNumbers (<)
    let le = compareNumbers (<=)

    let boolean x =
        match x with
        | Nil -> false
        | Boolean b -> b
        | _ -> true

    let cons xs =
        match xs with
        | [ x; List ys ] -> List (x :: ys)
        | [ _; expr ] -> LispError.wrongType expr "list"
        | _ -> LispError.wrongNumberOfArguments "cons" 2 xs

    let head xs =
        match xs with
        | [ List (head :: _) ] -> head
        | [ List [] ] -> Nil
        | [ expr ] -> LispError.wrongType expr "list"
        | _ -> LispError.wrongNumberOfArguments "head" 1 xs
    
    let tail xs =
        match xs with
        | [ List (_ :: tail) ] -> List tail
        | [ List [] ] -> List []
        | [ expr ] -> LispError.wrongType expr "list"
        | _ -> LispError.wrongNumberOfArguments "tail" 1 xs
    
    let private matchSpecialForm symbol fn expr =
        match expr with
        | List (Symbol s :: tail) when s = symbol -> Some (fn tail)
        | _ -> None

    let private (|DefinitionForm|_|) =
        matchSpecialForm Keyword.Definition (function
            | [ Symbol s; expr ] -> s, expr
            | [ List (head :: tail); expr ] -> toSymbol head, Lambda (tail |> List.map toSymbol, expr)
            | [ x; _ ] -> LispError.wrongType x "symbol"
            | xs -> LispError.wrongNumberOfArguments Keyword.Definition 2 xs)

    let private (|LambdaForm|_|) =
        matchSpecialForm Keyword.Lambda (function
            | [ List parameters; body ] -> Lambda ((parameters |> List.map toSymbol), body)
            | [ x; _ ] -> LispError.wrongType x "list"
            | xs -> LispError.wrongNumberOfArguments Keyword.Lambda 2 xs)

    let private (|ConditionalForm|_|) =
        matchSpecialForm Keyword.Conditional (function
            | [ condition; trueBranch; falseBranch ] -> condition, trueBranch, falseBranch
            | xs -> LispError.wrongNumberOfArguments Keyword.Conditional 3 xs)
    
    let private (|QuoteForm|_|) =
        matchSpecialForm Keyword.Quote (function
            | [ x ] -> x
            | xs -> LispError.wrongNumberOfArguments Keyword.Quote 1 xs)

    let rec eval (env: Environment) expr =
        match expr with
        | DefinitionForm (s, expr) ->
            env.Add(s, eval env expr)
            Nil
        | LambdaForm lambda -> lambda
        | ConditionalForm (condition, trueBranch, falseBranch) ->
            if condition |> eval env |> boolean then
                trueBranch
            else
                falseBranch
            |> eval env
        | QuoteForm list -> list
        | Symbol s -> env.Find(s)
        | List (head :: tail) ->
            let arguments = tail |> List.map (eval env)
            match eval env head with
            | Builtin f -> f arguments
            | Lambda (parameters, body) ->
                if parameters.Length = arguments.Length then
                    eval (env.CreateFunctionEnvironment(parameters, arguments)) body
                else LispError.wrongNumberOfArguments (string head) parameters.Length arguments
            | expr -> LispError.wrongType expr "function"
        | _ -> expr

module Parser =

    let private toOption<'a> (success, x: 'a) =
        if success then Some x else None
    
    let private (|Int|_|) (s: string) = s |> Int32.TryParse |> toOption

    let private (|Float|_|) (s: string) = s |> Double.TryParse |> toOption

    let private (|Boolean|_|) s =
        match s with
        | "true" -> Some true
        | "false" -> Some false
        | _ -> None

    let private tokenize (chars: string) =
        // wow
        // so lexer
        // many logic
        chars
            .Replace("(", " ( ")
            .Replace(")", " ) ")
            .Split([| ' '; '\t'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    let rec private readList acc xs =
        match xs with
        | ")" :: tail ->
            acc |> List.rev |> List, tail
        | tokens ->
            let expr, tail = readSExpr tokens
            readList (expr :: acc) tail

    and private readSExpr tokens =
        match tokens with
        | [] -> LispError.raise "Unexpected end of input."
        | ")" :: _ -> LispError.raise "Unexpected \")\"."
        | "(" :: tail -> readList [] tail
        | Int x :: tail -> Number (Int x), tail
        | Float x :: tail -> Number (Float x), tail
        | Boolean b :: tail -> Boolean b, tail
        | "nil" :: tail -> Nil, tail
        | s :: tail -> Symbol s, tail

    let parse input =
        try
            match input |> tokenize |> readSExpr with
            | expr, [] -> Ok expr
            | _ -> Error "The input is not a single expression."
        with
        | LispError message ->
            Error message
        | exn ->
            printfn "%A" exn
            Error "Something went wrong."

type Environment with

    member this.Eval(expr) =
        try SExpr.eval this expr |> Ok
        with
        | LispError s -> Error s
        | exn when exn.Message = "Maximum call stack size exceeded" ->
            Error "Stack overflow (no, not the website)."
        | exn ->
            printfn "%A" exn
            Error "Something went wrong."

module Environment =

    let private builtins =
        [ "+", SExpr.add
          "-", SExpr.sub
          "*", SExpr.mul
          "/", SExpr.div
          "%", SExpr.rem
          ">", SExpr.gt
          ">=", SExpr.ge
          "<", SExpr.lt
          "<=", SExpr.le
          "=", List.pairwise >> List.forall (fun (x, y) -> x = y) >> Boolean
          "<>", List.pairwise >> List.exists (fun (x, y) -> x <> y) >> Boolean
          "and", List.forall SExpr.boolean >> Boolean
          "or", List.exists SExpr.boolean >> Boolean
          "list", List
          "cons", SExpr.cons
          "head", SExpr.head
          "tail", SExpr.tail ]
        |> List.map (fun (s, v) -> (s, Builtin v))
        |> Map.ofList
    
    let private lambdas =
        [ "(def (not x) (if x false true))"
          "(def (abs x) (if (>= x 0) x (* x -1)))"
          "(def (fold f acc xs) (if (head xs) (fold f (f acc (head xs)) (tail xs)) acc))"
          "(def (reverse xs) (fold (fn (acc x) (cons x acc)) () xs))"
          "(def (count xs) (fold (fn (acc x) (+ acc 1)) 0 xs))"
          "(def (map g xs) (reverse (fold (fn (acc x) (cons (g x) acc)) () xs)))"
          "(def (filter g xs) (reverse (fold (fn (acc x) (if (g x) (cons x acc) acc)) () xs)))"
          "(def (range x y) (if (= x y) () (cons x (range (+ x 1) y))))" ]
        |> List.choose (fun x ->
            match Parser.parse x with
            | Ok expr -> Some expr
            | _ -> None)

    let createDefaultEnvironment() =
        let env = Environment(builtins)
        lambdas |> List.iter (env.Eval >> ignore)
        env