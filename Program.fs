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

module Keyword =

    [<Literal>]
    let Definition = "def"

    [<Literal>]
    let Lambda = "fn"

    [<Literal>]
    let Conditional = "if"

module SExpr =

    let private wrongType value expectedType =
        failwith $"\"{value}\" is not a {expectedType}."

    let private wrongNumberOfArguments name expected got =
        failwith $"\"{name}\" called with a wrong number of arguments (expected {expected}, got {List.length got})."

    let print x =  printfn $"{x}"

    let private toNumber = function
        | Number x -> x
        | x -> wrongType x "number"

    let private toSymbol = function
        | Symbol s -> s
        | x -> wrongType x "symbol"

    let private foldNumbers f x0 = List.map toNumber >> List.fold f x0 >> Number

    let private reduceNumbers f = List.map toNumber >> List.reduce f >> Number
    
    let private forAllNumbers f xs =
        match xs |> List.map toNumber with
        | head :: tail ->
            tail |> List.forall (f head) |> Boolean
        | [] -> Boolean false

    let add = foldNumbers Number.add (Int 0) |> Builtin
    let sub = reduceNumbers Number.sub |> Builtin
    let mul = foldNumbers Number.mul (Int 1) |> Builtin
    let div = reduceNumbers Number.div |> Builtin

    let gt = forAllNumbers Number.gt |> Builtin
    let ge = forAllNumbers Number.ge |> Builtin
    let lt = forAllNumbers Number.lt |> Builtin
    let le = forAllNumbers Number.le |> Builtin
    let eq = forAllNumbers Number.eq |> Builtin

    let head sexpr =
        match sexpr with
        | List (head :: _) -> head
        | List [] -> failwith "An empty list has no head."
        | _ -> wrongType sexpr "list"
    
    let tail sexpr =
        match sexpr with
        | List (_ :: tail) -> List tail
        | List [] -> failwith "An empty list has no tail."
        | _ -> wrongType sexpr "list"
    
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

    let rec eval (env: Environment) = function
        | DefinitionForm (s, sexpr) ->
            env.Add(s, eval env sexpr)
            List []
        | LambdaForm lambda -> lambda
        | ConditionalForm (condition, trueBranch, falseBranch) ->
            match eval env condition with
            | Boolean b -> if b then trueBranch else falseBranch
            | x -> wrongType x "boolean"
            |> eval env
        | List [ Symbol "quote"; sexpr ] -> sexpr
        | Symbol s -> env.Find(s)
        | List (head :: tail) ->
            let arguments = tail |> List.map (eval env)
            match eval env head with
            | Builtin f -> f arguments
            | Lambda (parameters, body) ->
                let functionEnv = Environment((parameters, arguments) ||> List.zip |> Map.ofList, Some env)
                eval functionEnv body
            | sexpr -> wrongType sexpr "function"
        | sexpr -> sexpr

module Parser =

    let private tokenize (chars: string) =
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

    let parse chars =
        match chars |> tokenize |> readSExpr with
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