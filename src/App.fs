module FsLisp.App

open Browser
open Browser.Types
open Feliz

let samples =
    [| "(def pi 3.14159)"
       "(def (square x) (* x x))"
       "(map square (range 1 10))" |]

type HistoryItem =
    { Input: string
      Expr: SExpr option
      Result: Result<SExpr, string>
      Env: IEnvironment }

type History =
    { Position: int
      Items: HistoryItem[] }

    member this.Add(item) =
        { Position = this.Items.Length + 1
          Items = [| yield! this.Items; yield item |] }

[<RequireQualifiedAccess>]
type SemanticInfo =
    | Lambda of expr: SExpr
    | Builtin
    | Keyword
    | Variable of expr: SExpr
    | Parameter
    | Unknown
    | FunctionDef

let getClassName (getSemanticInfo: string -> SemanticInfo) expr =
    match expr with
    | Nil _ -> "nil"
    | List _ -> "list"
    | Builtin _ -> "function"
    | Lambda _ -> "function"
    | Number _ -> "number"
    | Boolean _ -> "boolean"
    | Symbol s ->
        match getSemanticInfo s with
        | SemanticInfo.Keyword -> "keyword"
        | SemanticInfo.Lambda _
        | SemanticInfo.Builtin
        | SemanticInfo.FunctionDef -> "function"
        | SemanticInfo.Variable _
        | SemanticInfo.Parameter -> "variable"
        | SemanticInfo.Unknown -> "unknown"

let getSemanticInfoFromEnv (env: IEnvironment) s =
    if Keyword.isKeyWord s then
        SemanticInfo.Keyword
    else
        try
            let expr = env.Find(s)

            match expr with
            | Lambda _ -> SemanticInfo.Lambda expr
            | Builtin _ -> SemanticInfo.Builtin
            | _ -> SemanticInfo.Variable expr
        with _ ->
            SemanticInfo.Unknown

[<ReactComponent>]
let rec Expr (getSemanticInfo: string -> SemanticInfo) (expr: SExpr) =

    let className = getClassName getSemanticInfo expr

    match expr with
    | List(head :: tail) ->
        let parameters =
            match head, tail with
            | Symbol Keyword.Lambda, List(parameters) :: _
            | Symbol Keyword.Definition, List(_ :: parameters) :: _ ->
                try
                    parameters |> List.map SExpr.toSymbol |> Set.ofList
                with _ ->
                    Set.empty
            | Symbol Keyword.Definition, Symbol s :: _ -> Set.singleton s
            | _ -> Set.empty

        let functionName =
            match head, tail with
            | Symbol Keyword.Definition, List(Symbol s :: _) :: _ -> Some s
            | _ -> None

        let getSemanticInfo s =
            if Some s = functionName then SemanticInfo.FunctionDef
            elif parameters.Contains(s) then SemanticInfo.Parameter
            else getSemanticInfo s

        Html.span
            [ prop.className className
              prop.children
                  [ yield Html.span [ prop.text "(" ]
                    yield Expr getSemanticInfo head
                    for x in tail do
                        yield Html.span " "
                        yield Expr getSemanticInfo x
                    yield Html.span [ prop.text ")" ] ] ]
    | Lambda(lambdaEnv, parameters, body) ->
        List
            [ Symbol Keyword.Lambda
              List
                  [ for p in parameters do
                        Symbol p ]
              body ]
        |> Expr(getSemanticInfoFromEnv lambdaEnv)
    | Builtin _ -> Html.span [ prop.className "info"; prop.text "built-in" ]
    | Symbol s ->
        match getSemanticInfo s with
        | SemanticInfo.Lambda lambdaExpr ->
            Html.div
                [ prop.className "tooltip"
                  prop.children
                      [ Html.span [ prop.className className; prop.text (string expr) ]
                        Html.span [ prop.className "tooltip-content"; prop.text (string lambdaExpr) ] ] ]
        | SemanticInfo.Variable variableExpr ->
            Html.div
                [ prop.className "tooltip"
                  prop.children
                      [ Html.span [ prop.className className; prop.text (string expr) ]
                        Html.span [ prop.className "tooltip-content"; prop.text (string variableExpr) ] ] ]
        | _ -> Html.span [ prop.className className; prop.text (string expr) ]
    | _ -> Html.span [ prop.className className; prop.text (string expr) ]

[<ReactComponent>]
let EnvTable (env: IEnvironment) onSymbolClick =

    let getSemanticInfo = getSemanticInfoFromEnv env

    let getValueElement expr =

        let className = expr |> getClassName getSemanticInfo

        match expr with
        | Builtin _ -> Html.span [ prop.className "info"; prop.text "built-in" ]
        | Nil -> Html.span [ prop.className className; prop.text "nil" ]
        | List _ -> Html.span [ prop.className className; prop.text "list" ]
        | Lambda _ -> Html.span [ prop.className "info"; prop.text "lambda" ]
        | Symbol s -> Html.span [ prop.className className; prop.text s ]
        | Number _
        | Boolean _ -> Expr getSemanticInfo expr

    let children =
        [ for (symbol, expr) in env.ToArray() do
              Html.tr
                  [ Html.td
                        [ prop.className (Symbol symbol |> getClassName getSemanticInfo)
                          prop.onClick (fun _ -> onSymbolClick symbol)
                          prop.children [ Html.div [ prop.text (string symbol) ] ] ]
                    Html.td [ getValueElement expr ] ] ]

    Html.div
        [ prop.className "env-table"
          prop.children [ Html.table [ Html.tbody children ] ] ]

[<ReactComponent>]
let HistoryBrowser history onItemClick =

    let ref = React.useRef None

    React.useEffect (fun () ->
        ref.current
        |> Option.iter (fun current ->
            let element = unbox<HTMLDivElement> current
            element.scrollTop <- element.scrollHeight))

    Html.div
        [ prop.ref ref
          prop.className "history-browser"
          prop.children
              [ for item in history.Items do
                    Html.dl
                        [ Html.dt
                              [ prop.onClick (fun _ -> onItemClick item.Input)
                                prop.children
                                    [ match item.Expr with
                                      | Some expr -> Expr (getSemanticInfoFromEnv item.Env) expr
                                      | None -> Html.span item.Input ] ]
                          match item.Result with
                          | Error message ->
                              Html.dd [ Html.span [ prop.className "error"; prop.text (string message) ] ]
                          | Ok expr -> Html.dd [ Expr (getSemanticInfoFromEnv item.Env) expr ] ] ] ]

[<ReactComponent>]
let Repl (env: IEnvironment) =
    let (input, setInput) = React.useState ""

    let (history, updateHistory) =
        React.useStateWithUpdater ({ Position = 0; Items = [||] })

    let inputRef = React.useRef None

    let update input =
        setInput ""

        match Parser.parse input with
        | Ok expr ->
            updateHistory (fun history ->
                history.Add(
                    { Input = input
                      Expr = Some expr
                      Result = env.Eval(expr)
                      Env = env.Copy() }
                ))
        | Error message ->
            updateHistory (fun history ->
                history.Add(
                    { Input = input
                      Expr = None
                      Result = Error message
                      Env = env.Copy() }
                ))

    React.useEffectOnce (fun () -> samples |> Array.iter update)

    React.useEffect (fun () ->
        inputRef.current
        |> Option.iter (fun current ->
            let element = unbox<HTMLInputElement> current
            element.focus ()))

    let onInputKeyDown (e: KeyboardEvent) =
        let direction =
            match e.key with
            | "ArrowUp" -> -1
            | "ArrowDown" -> 1
            | _ -> 0

        if direction <> 0 then
            e.preventDefault ()
            let items = history.Items
            let position = max 0 (min (history.Position + direction) items.Length)

            if position < items.Length then
                setInput items.[position].Input
            else
                setInput ""

            updateHistory (fun history -> { history with Position = position })

    React.fragment
        [ EnvTable env setInput
          Html.form
              [ prop.className "repl"
                prop.spellcheck false
                prop.onSubmit (fun e ->
                    e.preventDefault ()
                    update input)
                prop.children
                    [ HistoryBrowser history setInput
                      Html.input
                          [ prop.ref inputRef
                            prop.type' "text"
                            prop.value input
                            prop.onChange setInput
                            prop.onKeyDown onInputKeyDown ] ] ] ]

let env: IEnvironment = Environment.createDefaultEnvironment ()

ReactDOM.render (Repl(env), document.getElementById "root")
