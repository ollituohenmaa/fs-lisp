namespace FsLisp

open System

exception LispError of message: string with
    static member raise(message) = raise (LispError message)

type Number =
    | Int of int
    | Float of float

    override this.ToString() =
        match this with
        | Int x -> string x
        | Float x -> string x

module Number =

    let private operator fnInt fnFloat x y =
        match x, y with
        | Int x, Int y -> fnInt x y |> Int
        | Int x, Float y -> fnFloat (float x) y |> Float
        | Float x, Int y -> fnFloat x (float y) |> Float
        | Float x, Float y -> fnFloat x y |> Float
    
    let private predicate fnInt fnFloat x y =
        match x, y with
        | Int x, Int y -> fnInt x y
        | Int x, Float y -> fnFloat (float x) y
        | Float x, Int y -> fnFloat x (float y)
        | Float x, Float y -> fnFloat x y

    let add = operator (+) (+)
    let sub = operator (-) (-)
    let mul = operator (*) (*)
    let div = operator (/) (/)

    let gt = predicate (>) (>)
    let ge = predicate (>=) (>=)
    let lt = predicate (<) (<)
    let le = predicate (<=) (<=)
    let eq = predicate (=) (=)

type SExpr =
    | Symbol of string
    | Number of Number
    | Boolean of bool
    | Builtin of (SExpr list -> SExpr)
    | Lambda of parameters: string list * body: SExpr
    | List of SExpr list

    override this.ToString() =
        match this with
        | Symbol s -> s
        | Number x -> string x
        | Boolean b -> if b then "true" else "false"
        | Builtin f -> "<function>"
        | Lambda (parameters, body) ->
            sprintf "(fn (%s) %s)" (String.Join(" ", parameters)) (string body)
        | List [] -> "()"
        | List xs -> $"""({String.Join(" ", xs)})"""

type Environment(map: Map<string, SExpr>, ?parent: Environment) =

    let mutable map = map

    member _.Add(symbol, expr) =
        map <- map.Add(symbol, expr)

    member _.Find(s) =
        match map.TryFind(s), parent with
        | Some value, _ -> value
        | None, Some parent -> parent.Find(s)
        | None, _ -> LispError.raise $"\"{s}\" is undefined."
    
    member this.CreateFunctionEnvironment(parameters, arguments) =
        Environment((parameters, arguments) ||> List.zip |> Map.ofList, this)

module SExpr =

    module private Keyword =

        [<Literal>]
        let Definition = "def"

        [<Literal>]
        let Lambda = "fn"

        [<Literal>]
        let Conditional = "if"

        [<Literal>]
        let Quote = "quote"

    let private wrongType value expectedType =
        LispError.raise $"\"{value}\" is not a {expectedType}."

    let private wrongNumberOfArguments name expected got =
        LispError.raise $"\"{name}\" called with a wrong number of arguments (expected {expected}, got {List.length got})."

    let private toSymbol expr =
        match expr with
        | Symbol s -> s
        | _ -> wrongType expr "symbol"

    let private liftOperator operator (s: SExpr) (x: SExpr) =
        match s, x with
        | Number s, Number x -> (s, x) ||> operator
        | Number _, x -> wrongType x "number"
        | s, _ -> wrongType s "number"

    let private foldNumbers operator x0 = List.fold (liftOperator (fun s x -> Number (operator s x))) x0

    let private reduceNumbers operator = List.reduce (liftOperator (fun s x -> Number (operator s x)))
    
    let private compareNumbers predicate xs =
        match xs with
        | head :: tail -> tail |> List.forall (liftOperator predicate head) |> Boolean
        | [] -> Boolean false

    let add = foldNumbers Number.add (Number (Int 0))
    let sub = reduceNumbers Number.sub
    let mul = foldNumbers Number.mul (Number (Int 1))
    let div = reduceNumbers Number.div

    let gt = compareNumbers Number.gt
    let ge = compareNumbers Number.ge
    let lt = compareNumbers Number.lt
    let le = compareNumbers Number.le
    let eq = compareNumbers Number.eq

    let head xs =
        match xs with
        | [ List (head :: _) ] -> head
        | [ List []] -> LispError.raise "An empty list has no head."
        | [ expr ] -> wrongType expr "list"
        | _ -> wrongNumberOfArguments "head" 1 xs
    
    let tail xs =
        match xs with
        | [ List (_ :: tail) ] -> List tail
        | [ List [] ] -> LispError.raise "An empty list has no tail."
        | [ expr ] -> wrongType expr "list"
        | _ -> wrongNumberOfArguments "tail" 1 xs
    
    let private matchSpecialForm symbol fn expr =
        match expr with
        | List (Symbol s :: tail) when s = symbol -> Some (fn tail)
        | _ -> None

    let private (|DefinitionForm|_|) =
        matchSpecialForm Keyword.Definition (function
            | [ Symbol s; expr ] -> s, expr
            | [ x; _ ] -> wrongType x "symbol"
            | xs -> wrongNumberOfArguments Keyword.Definition 2 xs)

    let private (|LambdaForm|_|) =
        matchSpecialForm Keyword.Lambda (function
            | [ List parameters; body ] -> Lambda ((parameters |> List.map toSymbol), body)
            | [ x; _ ] -> wrongType x "list"
            | xs -> wrongNumberOfArguments Keyword.Lambda 2 xs)

    let private (|ConditionalForm|_|) =
        matchSpecialForm Keyword.Conditional (function
            | [ condition; trueBranch; falseBranch ] -> condition, trueBranch, falseBranch
            | xs -> wrongNumberOfArguments Keyword.Conditional 3 xs)
    
    let private (|QuoteForm|_|) =
        matchSpecialForm Keyword.Quote (function
            | [ x ] -> x
            | xs -> wrongNumberOfArguments Keyword.Quote 1 xs)

    let rec eval (env: Environment) expr =
        match expr with
        | DefinitionForm (s, expr) ->
            env.Add(s, eval env expr)
            List []
        | LambdaForm lambda -> lambda
        | ConditionalForm (condition, trueBranch, falseBranch) ->
            match eval env condition with
            | Boolean true -> eval env trueBranch
            | Boolean false -> eval env falseBranch
            | x -> wrongType x "boolean"
        | QuoteForm list -> list
        | Symbol s -> env.Find(s)
        | List (head :: tail) ->
            let arguments = tail |> List.map (eval env)
            match eval env head with
            | Builtin f -> f arguments
            | Lambda (parameters, body) -> eval (env.CreateFunctionEnvironment(parameters, arguments)) body
            | expr -> wrongType expr "function"
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
        | head :: tail when head = ")" ->
            acc |> List.rev |> List, tail
        | tokens ->
            let expr, tail = readSExpr tokens
            readList (expr :: acc) tail

    and private readSExpr tokens =
        match tokens with
        | [] ->
            LispError.raise "Unexpected end of input."
        | head :: tail ->
            match head with
            | "(" -> readList [] tail
            | ")" -> LispError.raise "Unexpected \")\"."
            | Int x -> Number (Int x), tail
            | Float x -> Number (Float x), tail
            | Boolean b -> Boolean b, tail
            | s -> Symbol s, tail

    let parse input =
        try
            match input |> tokenize |> readSExpr with
            | expr, [] -> Ok expr
            | _ -> Error "The input is not a single expression."
        with _ -> Error "Something went wrong."
        

type Environment with

    member this.Eval(expr) =
        try SExpr.eval this expr |> Ok
        with
        | LispError s -> Error s
        | _ -> Error "Something went wrong."

module Environment =

    let private builtins =
        [ "+", SExpr.add
          "-", SExpr.sub
          "*", SExpr.mul
          "/", SExpr.div
          ">", SExpr.gt
          ">=", SExpr.ge
          "<", SExpr.lt
          "<=", SExpr.le
          "=", SExpr.eq
          "do", List.last
          "head", SExpr.head
          "tail", SExpr.tail ]
        |> List.map (fun (s, v) -> (s, Builtin v))
        |> Map.ofList

    let createDefaultEnvironment() = Environment(builtins)