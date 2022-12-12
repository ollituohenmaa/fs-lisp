namespace FsLisp

open System

exception LispError of message: string with

    static member raise message = raise (LispError message)

    static member wrongType value expectedType =
        LispError.raise $"Type mismatch: {string value} is not a {expectedType}."

    static member wrongNumberOfArguments name expected got =
        LispError.raise $"Wrong number of arguments for {name}: expected {expected}, got {List.length got}."

module Keywords =

    [<Literal>]
    let Definition = "def"

    [<Literal>]
    let Let = "let"

    [<Literal>]
    let Lambda = "fn"

    [<Literal>]
    let Conditional = "if"

    [<Literal>]
    let Quote = "quote"

    [<Literal>]
    let Eval = "eval"

    [<Literal>]
    let And = "and"

    [<Literal>]
    let Or = "or"

    let private keywords =
        Set([ Definition; Let; Lambda; Conditional; Quote; Eval; And; Or ])

    let contains = keywords.Contains

    let asArray = keywords |> Set.toArray

[<CustomEquality; NoComparison>]
type SExpr =
    | Symbol of string
    | Number of Number
    | Boolean of bool
    | Builtin of (SExpr list -> SExpr)
    | Lambda of env: IEnvironment * parameters: string list * body: SExpr
    | List of SExpr list
    | Nil

    override this.ToString() =
        match this with
        | Symbol s -> s
        | Number x -> string x
        | Boolean b -> if b then "true" else "false"
        | Builtin _ -> "built-in"
        | Lambda(_, parameters, body) -> sprintf "(fn (%s) %s)" (String.Join(" ", parameters)) (string body)
        | List xs -> sprintf "(%s)" (String.Join(" ", xs))
        | Nil -> "nil"

    interface IEquatable<SExpr> with

        member this.Equals(other) =
            match this, other with
            | Symbol a, Symbol b -> a = b
            | Number a, Number b -> a = b
            | Boolean a, Boolean b -> a = b
            | List a, List b -> a = b
            | Nil, Nil -> true
            | _ -> false

    override this.Equals(other) =
        match other with
        | :? SExpr as other -> (this :> IEquatable<_>).Equals(other)
        | _ -> false

    override _.GetHashCode() = raise (NotImplementedException())

and IEnvironment =
    abstract Add: string * SExpr -> unit
    abstract AddToRoot: string * SExpr -> unit
    abstract Find: string -> SExpr
    abstract CreateChild: ?bindings: list<string * SExpr> -> IEnvironment
    abstract Copy: unit -> IEnvironment
    abstract ToArray: unit -> (string * SExpr)[]
    abstract Eval: SExpr -> Result<SExpr, string>

module SExpr =

    let toSymbol expr =
        match expr with
        | Symbol s -> s
        | _ -> LispError.wrongType expr "symbol"

    let private toBoolean x =
        match x with
        | Nil -> false
        | Boolean b -> b
        | _ -> true

    let private matchSpecialForm symbol fn expr =
        match expr with
        | List(Symbol s :: tail) when s = symbol -> Some(fn tail)
        | _ -> None

    let private (|DefinitionForm|_|) env =
        matchSpecialForm Keywords.Definition (function
            | [ Symbol s; expr ] -> s, expr
            | [ List(head :: tail); expr ] -> toSymbol head, Lambda(env, tail |> List.map toSymbol, expr)
            | [ x; _ ] -> LispError.wrongType x "symbol"
            | xs -> LispError.wrongNumberOfArguments Keywords.Definition 2 xs)

    [<RequireQualifiedAccess>]
    type LetBinding =
        | Variable of name: string * value: SExpr * body: SExpr
        | Lambda of lambdaName: string * lambdaParameters: list<string> * lambdaBody: SExpr * body: SExpr

    let private (|LetForm|_|) =
        matchSpecialForm Keywords.Let (function
            | [ List [ Symbol name; value ]; body ] -> LetBinding.Variable(name, value, body)
            | [ List [ List(Symbol lambdaName :: lambdaParameters); lambdaBody ]; body ] ->
                LetBinding.Lambda(lambdaName, lambdaParameters |> List.map toSymbol, lambdaBody, body)
            | [ List [ x; _ ]; _ ] -> LispError.wrongType x "symbol"
            | [ _; _ ] -> LispError.raise "The first argument should be a list with 2 elements."
            | xs -> LispError.wrongNumberOfArguments Keywords.Let 2 xs)

    let private (|LambdaForm|_|) env =
        matchSpecialForm Keywords.Lambda (function
            | [ List parameters; body ] -> Lambda(env, (parameters |> List.map toSymbol), body)
            | [ x; _ ] -> LispError.wrongType x "list"
            | xs -> LispError.wrongNumberOfArguments Keywords.Lambda 2 xs)

    let private (|ConditionalForm|_|) =
        matchSpecialForm Keywords.Conditional (function
            | [ condition; trueBranch; falseBranch ] -> condition, trueBranch, falseBranch
            | xs -> LispError.wrongNumberOfArguments Keywords.Conditional 3 xs)

    let private (|QuoteForm|_|) =
        matchSpecialForm Keywords.Quote (function
            | [ x ] -> x
            | xs -> LispError.wrongNumberOfArguments Keywords.Quote 1 xs)

    let private (|EvalForm|_|) =
        matchSpecialForm Keywords.Eval (function
            | [ x ] -> x
            | xs -> LispError.wrongNumberOfArguments Keywords.Eval 1 xs)

    let private (|AndForm|_|) = matchSpecialForm Keywords.And id

    let private (|OrForm|_|) = matchSpecialForm Keywords.Or id

    let rec eval (env: IEnvironment) expr =
        match expr with
        | DefinitionForm env (s, expr) ->
            if Keywords.contains s then
                raise (LispError $"{s} is a reserved keyword.")
            else
                env.AddToRoot(s, eval env expr)
                Nil
        | LetForm binding ->
            let childEnv = env.CreateChild()

            let name, value, body =
                match binding with
                | LetBinding.Variable(name, value, body) -> name, eval env value, body
                | LetBinding.Lambda(lambdaName, lambdaParameters, lambdaBody, body) ->
                    lambdaName, Lambda(childEnv, lambdaParameters, lambdaBody), body

            childEnv.Add(name, value)
            eval childEnv body
        | LambdaForm env lambda -> lambda
        | ConditionalForm(condition, trueBranch, falseBranch) ->
            if condition |> eval env |> toBoolean then
                trueBranch
            else
                falseBranch
            |> eval env
        | QuoteForm expr -> expr
        | EvalForm expr -> expr |> eval env |> eval env
        | AndForm xs -> xs |> List.forall (eval env >> toBoolean) |> Boolean
        | OrForm xs -> xs |> List.exists (eval env >> toBoolean) |> Boolean
        | Symbol s -> env.Find(s)
        | List(head :: tail) ->
            let arguments = tail |> List.map (eval env)

            match eval env head with
            | Builtin f -> f arguments
            | Lambda(lambdaEnv, parameters, body) ->
                if parameters.Length = arguments.Length then
                    eval (lambdaEnv.CreateChild(List.zip parameters arguments)) body
                else
                    LispError.wrongNumberOfArguments (string head) parameters.Length arguments
            | expr -> LispError.wrongType expr "function"
        | _ -> expr
