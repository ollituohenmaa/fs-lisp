namespace FsLisp

open System

exception LispError of message: string with

    static member raise message = raise (LispError message)

    static member wrongType value expectedType =
        LispError.raise $"Type mismatch: {value} is not a {expectedType}."

    static member wrongNumberOfArguments name expected got =
        LispError.raise $"Wrong number of arguments for {name}: expected {expected}, got {List.length got}."

module Keyword =

    [<Literal>]
    let Definition = "def"

    [<Literal>]
    let Lambda = "fn"

    [<Literal>]
    let Conditional = "if"

    [<Literal>]
    let Quote = "quote"

    let private keywords = Set([ Definition; Lambda; Conditional; Quote ])

    let isKeyWord = keywords.Contains

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
        | Builtin _ -> "built-in"
        | Lambda(parameters, body) -> sprintf "(fn (%s) %s)" (String.Join(" ", parameters)) (string body)
        | List xs -> sprintf "(%s)" (String.Join(" ", xs))
        | Nil -> "nil"

    interface IEquatable<SExpr> with

        member this.Equals(other) =
            match this, other with
            | Builtin _, Builtin _ -> false
            | Symbol a, Symbol b -> a = b
            | Number a, Number b -> a = b
            | Boolean a, Boolean b -> a = b
            | Lambda(a1, a2), Lambda(b1, b2) -> a1 = b1 && a2 = b2
            | List a, List b -> a = b
            | Nil, Nil -> true
            | _ -> false

    override this.Equals(other) =
        match other with
        | :? SExpr as other -> (this :> IEquatable<_>).Equals(other)
        | _ -> false

    override _.GetHashCode() = raise (NotImplementedException())

type IEnvironment =
    abstract Add: string * SExpr -> unit
    abstract Find: string -> SExpr
    abstract CreateFunctionEnvironment: list<string> * list<SExpr> -> IEnvironment

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

    let private foldNumbers operator x0 =
        List.fold (liftOperator (fun s x -> Number(operator s x))) x0

    let private reduceNumbers operator =
        List.reduce (liftOperator (fun s x -> Number(operator s x)))

    let private compareNumbers predicate xs =
        match xs with
        | head :: tail -> tail |> List.forall (liftOperator predicate head) |> Boolean
        | [] -> Boolean false

    let add = foldNumbers (+) (Number(Int 0))
    let sub = reduceNumbers (-)
    let mul = foldNumbers (*) (Number(Int 1))
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
        | [ x; List ys ] -> List(x :: ys)
        | [ _; expr ] -> LispError.wrongType expr "list"
        | _ -> LispError.wrongNumberOfArguments "cons" 2 xs

    let head xs =
        match xs with
        | [ List(head :: _) ] -> head
        | [ List [] ] -> Nil
        | [ expr ] -> LispError.wrongType expr "list"
        | _ -> LispError.wrongNumberOfArguments "head" 1 xs

    let tail xs =
        match xs with
        | [ List(_ :: tail) ] -> List tail
        | [ List [] ] -> List []
        | [ expr ] -> LispError.wrongType expr "list"
        | _ -> LispError.wrongNumberOfArguments "tail" 1 xs

    let private matchSpecialForm symbol fn expr =
        match expr with
        | List(Symbol s :: tail) when s = symbol -> Some(fn tail)
        | _ -> None

    let private (|DefinitionForm|_|) =
        matchSpecialForm Keyword.Definition (function
            | [ Symbol s; expr ] -> s, expr
            | [ List(head :: tail); expr ] -> toSymbol head, Lambda(tail |> List.map toSymbol, expr)
            | [ x; _ ] -> LispError.wrongType x "symbol"
            | xs -> LispError.wrongNumberOfArguments Keyword.Definition 2 xs)

    let private (|LambdaForm|_|) =
        matchSpecialForm Keyword.Lambda (function
            | [ List parameters; body ] -> Lambda((parameters |> List.map toSymbol), body)
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

    let rec eval (env: IEnvironment) expr =
        match expr with
        | DefinitionForm(s, expr) ->
            env.Add(s, eval env expr)
            Nil
        | LambdaForm lambda -> lambda
        | ConditionalForm(condition, trueBranch, falseBranch) ->
            if condition |> eval env |> boolean then
                trueBranch
            else
                falseBranch
            |> eval env
        | QuoteForm list -> list
        | Symbol s -> env.Find(s)
        | List(head :: tail) ->
            let arguments = tail |> List.map (eval env)

            match eval env head with
            | Builtin f -> f arguments
            | Lambda(parameters, body) ->
                if parameters.Length = arguments.Length then
                    eval (env.CreateFunctionEnvironment(parameters, arguments)) body
                else
                    LispError.wrongNumberOfArguments (string head) parameters.Length arguments
            | expr -> LispError.wrongType expr "function"
        | _ -> expr