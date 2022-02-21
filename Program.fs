module FsLisp

open System

type Number =
    | Int of int
    | Float of float

    override this.ToString() =
        match this with
        | Int x -> $"{x}"
        | Float x -> $"{x}"

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
        | Number x -> $"{x}"
        | Boolean b -> if b then "true" else "false"
        | Builtin f -> "<function>"
        | Lambda (parameters, body) -> $"(fn ({String.Join(' ', parameters)}) {body})"
        | List [] -> "()"
        | List xs -> $"({String.Join(' ', xs)})"

module Primitive =

    let private tryParse (parser: string -> bool * _) =
        parser >> function
        | true, x -> Some x
        | false, _ -> None
    
    let private (|Int|_|) = tryParse Int32.TryParse

    let private (|Float|_|) = tryParse Double.TryParse

    let private (|Boolean|_|) = function
        | "true" -> Some true
        | "false" -> Some false
        | _ -> None

    let parse = function
        | Int x -> Int x |> Number
        | Float x -> Float x |> Number
        | Boolean b -> Boolean b
        | s -> Symbol s

type Environment(map: Map<string, SExpr>, parent: Environment option) =

    let mutable map = map

    member _.Add(symbol, sexpr) =
        map <- map.Add(symbol, sexpr)

    member _.Find(s) =
        match map.TryFind(s), parent with
        | Some value, _ -> value
        | None, Some parent -> parent.Find(s)
        | None, _ -> failwith $"\"{s}\" is undefined."
    
    member this.CreateFunctionEnvironment(parameters, arguments) =
        Environment((parameters, arguments) ||> List.zip |> Map.ofList, Some this)

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

    let private wrongType value expectedType =
        failwith $"\"{value}\" is not a {expectedType}."

    let private wrongNumberOfArguments name expected got =
        failwith $"\"{name}\" called with a wrong number of arguments (expected {expected}, got {List.length got})."

    let print x =  printfn $"{x}"

    let private toSymbol = function
        | Symbol s -> s
        | x -> wrongType x "symbol"

    let private liftOperator operator wrap (s: SExpr) (x: SExpr) =
        match s, x with
        | Number s, Number x -> wrap (operator s x)
        | Number _, x -> wrongType x "number"
        | s, _ -> wrongType s "number"

    let private foldNumbers operator x0 = List.fold (liftOperator operator Number) x0

    let private reduceNumbers operator = List.reduce (liftOperator operator Number)
    
    let private forAllNumbers predicate = function
        | head :: tail -> tail |> List.forall (liftOperator predicate id head) |> Boolean
        | [] -> Boolean false

    let add = foldNumbers Number.add (Number (Int 0)) |> Builtin
    let sub = reduceNumbers Number.sub |> Builtin
    let mul = foldNumbers Number.mul (Number (Int 1)) |> Builtin
    let div = reduceNumbers Number.div |> Builtin

    let gt = forAllNumbers Number.gt |> Builtin
    let ge = forAllNumbers Number.ge |> Builtin
    let lt = forAllNumbers Number.lt |> Builtin
    let le = forAllNumbers Number.le |> Builtin
    let eq = forAllNumbers Number.eq |> Builtin

    let head = function
        | List (head :: _) -> head
        | List [] -> failwith "An empty list has no head."
        | sexpr -> wrongType sexpr "list"
    
    let tail = function
        | List (_ :: tail) -> List tail
        | List [] -> failwith "An empty list has no tail."
        | sexpr -> wrongType sexpr "list"
    
    let private matchSpecialForm symbol = function
        | List (Symbol s :: tail) when s = symbol -> Some tail
        | _ -> None

    let private (|DefinitionForm|_|) =
        matchSpecialForm Keyword.Definition >> (Option.map (function
            | [ Symbol s; sexpr ] -> s, sexpr
            | [ x; _ ] -> wrongType x "symbol"
            | xs -> wrongNumberOfArguments Keyword.Definition 2 xs))

    let private (|LambdaForm|_|) =
        matchSpecialForm Keyword.Lambda >> (Option.map (function
            | [ List parameters; body ] -> Lambda ((parameters |> List.map toSymbol), body)
            | [ x; _ ] -> wrongType x "list"
            | xs -> wrongNumberOfArguments Keyword.Lambda 2 xs))

    let private (|ConditionalForm|_|) =
        matchSpecialForm Keyword.Conditional >> (Option.map (function
            | [ condition; trueBranch; falseBranch ] -> condition, trueBranch, falseBranch
            | xs -> wrongNumberOfArguments Keyword.Conditional 3 xs))
    
    let private (|QuoteForm|_|) =
        matchSpecialForm Keyword.Quote >> (Option.map (function
            | [ List _ as list ] -> list
            | [ x ] -> wrongType x "list"
            | xs -> wrongNumberOfArguments Keyword.Quote 1 xs))

    let rec eval (env: Environment) = function
        | DefinitionForm (s, sexpr) ->
            env.Add(s, eval env sexpr)
            List []
        | LambdaForm lambda -> lambda
        | ConditionalForm (condition, trueBranch, falseBranch) ->
            match eval env condition with
            | Boolean b -> (if b then trueBranch else falseBranch) |> eval env
            | x -> wrongType x "boolean"
        | QuoteForm list -> list
        | Symbol s -> env.Find(s)
        | List (head :: tail) ->
            let arguments = tail |> List.map (eval env)
            match eval env head with
            | Builtin f -> f arguments
            | Lambda (parameters, body) -> eval (env.CreateFunctionEnvironment(parameters, arguments)) body
            | sexpr -> wrongType sexpr "function"
        | sexpr -> sexpr

module Parser =

    let private tokenize (chars: string) =
        // wow
        // so lexer
        // many logic
        chars
            .Replace("(", " ( ")
            .Replace(")", " ) ")
            .Split([| ' '; '\t'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    let rec private readList acc = function
        | head :: tail when head = ")" ->
            acc |> List.rev |> List, tail
        | tokens ->
            let sexpr, tail = readSExpr tokens
            readList (sexpr :: acc) tail

    and private readSExpr = function
        | [] ->
            failwith "Unexpected end of input."
        | head :: tail ->
            match head with
            | "(" ->
                readList [] tail
            | ")" ->
                failwith "Unexpected \")\"."
            | _ ->
                Primitive.parse head, tail

    let parse =
        tokenize >> readSExpr >> function
        | sexpr, [] -> sexpr
        | _ -> failwith "The input is not a single expression."

let builtins =
    [ "+", SExpr.add
      "-", SExpr.sub
      "*", SExpr.mul
      "/", SExpr.div
      ">", SExpr.gt
      ">=", SExpr.ge
      "<", SExpr.lt
      "<=", SExpr.le
      "=", SExpr.eq
      "do", Builtin List.last
      "head", Builtin (List.head >> SExpr.head)
      "tail", Builtin (List.head >> SExpr.tail) ]
    |> Map.ofList

let globalEnv = Environment(builtins, None)

while true do
    try
        ReadLine.Read(">>= ")
        |> Parser.parse
        |> SExpr.eval globalEnv
        |> SExpr.print
    with e ->
        printfn $"Error: {e.Message}"