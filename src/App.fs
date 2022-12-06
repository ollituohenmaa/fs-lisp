module FsLisp.App

open Browser
open Browser.Types
open Feliz

let samples =
    [| "(def pi 3.14159)"
       "(def (square x) (* x x))"
       "(map square (range 1 10))" |]

type History =
    { Position: int
      Items: (string * SExpr option * Result<SExpr, string>)[] }

    member this.Add(item) =
        { Position = this.Items.Length + 1
          Items = [| yield! this.Items; yield item |] }

let getClassName semanticInfo =
    match semanticInfo with
    | SemanticInfo.Nil -> "nil"
    | SemanticInfo.List -> "list"
    | SemanticInfo.Function -> "function"
    | SemanticInfo.Number -> "number"
    | SemanticInfo.Boolean -> "boolean"
    | SemanticInfo.Keyword -> "keyword"
    | SemanticInfo.Variable -> "variable"
    | SemanticInfo.Unknown -> "unknown"

[<ReactComponent>]
let rec Expr (getSemanticInfo: SExpr -> SemanticInfo) (expr: SExpr) =

    let className = expr |> getSemanticInfo |> getClassName

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

        let getSemanticInfo expr =
            match expr with
            | Symbol s when parameters.Contains(s) -> SemanticInfo.Variable
            | _ -> getSemanticInfo expr

        Html.span
            [ prop.className className
              prop.children
                  [ yield Html.span [ prop.text "(" ]
                    yield Expr getSemanticInfo head
                    for x in tail do
                        yield Html.span " "
                        yield Expr getSemanticInfo x
                    yield Html.span [ prop.text ")" ] ] ]
    | Lambda(parameters, body) ->
        List
            [ Symbol Keyword.Lambda
              List
                  [ for p in parameters do
                        Symbol p ]
              body ]
        |> Expr getSemanticInfo
    | _ -> Html.span [ prop.className className; prop.text (string expr) ]

[<ReactComponent>]
let EnvTable (env: Environment) onSymbolClick =

    let getValueElement expr =

        let className = expr |> env.GetSemanticInfo |> getClassName

        match expr with
        | Builtin _ -> Html.span [ prop.className className; prop.text "built-in" ]
        | Nil -> Html.span [ prop.className className; prop.text "nil" ]
        | List _ -> Html.span [ prop.className className; prop.text "list" ]
        | Lambda _ -> Html.span [ prop.className className; prop.text "lambda" ]
        | Symbol s -> Html.span [ prop.className className; prop.text s ]
        | Number _
        | Boolean _ -> Expr env.GetSemanticInfo expr

    let children =
        [ for (symbol, expr) in env.ToArray() do
              Html.tr
                  [ Html.td
                        [ prop.title symbol
                          prop.onClick (fun _ -> onSymbolClick symbol)
                          prop.children [ Html.div [ prop.text (string symbol) ] ] ]
                    Html.td [ getValueElement expr ] ] ]

    Html.div
        [ prop.className "env-table"
          prop.children [ Html.table [ Html.tbody children ] ] ]

[<ReactComponent>]
let HistoryBrowser getSemanticInfo history onItemClick =

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
              [ for (input, expr, result) in history.Items do
                    Html.dl
                        [ Html.dt
                              [ prop.onClick (fun _ -> onItemClick input)
                                prop.children
                                    [ match expr with
                                      | Some expr -> Expr getSemanticInfo expr
                                      | None -> Html.span input ] ]
                          match result with
                          | Error message ->
                              Html.dd [ Html.span [ prop.className "error"; prop.text (string message) ] ]
                          | Ok expr -> Html.dd [ Expr getSemanticInfo expr ] ] ] ]

[<ReactComponent>]
let Repl (env: Environment) =
    let (input, setInput) = React.useState ""

    let (history, updateHistory) =
        React.useStateWithUpdater ({ Position = 0; Items = [||] })

    let inputRef = React.useRef None

    let update input =
        setInput ""

        match Parser.parse input with
        | Ok expr ->
            let result = env.Eval(expr)
            updateHistory (fun history -> history.Add(input, Some expr, result))
        | Error message -> updateHistory (fun history -> history.Add(input, None, Error message))

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
                let input, _, _ = items.[position]
                setInput input
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
                    [ HistoryBrowser env.GetSemanticInfo history setInput
                      Html.input
                          [ prop.ref inputRef
                            prop.type' "text"
                            prop.value input
                            prop.onChange setInput
                            prop.onKeyDown onInputKeyDown ] ] ] ]

let env = Environment.createDefaultEnvironment ()

ReactDOM.render (Repl(env), document.getElementById "root")
