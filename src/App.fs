module FsLisp.App

open Browser
open Browser.Types
open Feliz

let samples =
    [| "(def (square x) (* x x))"
       "(map square (range 1 10))" |]

let keywords =
    Set([ Keyword.Definition
          Keyword.Lambda
          Keyword.Conditional
          Keyword.Quote ])

type History =
    { Position: int
      Items: (string * SExpr option * Result<SExpr, string>)[] }

    member this.Add(item) =
        { Position = this.Items.Length + 1
          Items = [| yield! this.Items; yield item |] }

[<ReactComponent>]
let rec Expr (depth: int) (expr: SExpr) =

    let className =
        match expr with
        | Builtin _ -> "builtin"
        | Nil -> "nil"
        | List _ -> $"list-{depth % 2}"
        | Lambda _ -> ""
        | Number _ -> "number"
        | Boolean _ -> "boolean"
        | Symbol x when keywords.Contains(x) -> "keyword"
        | Symbol _ -> "symbol"

    match expr with
    | List (head :: tail) ->
        Html.span [
            prop.className className
            prop.children [
                yield Html.span [ prop.text "(" ]
                yield Expr (depth + 1) head
                for x in tail do
                    yield Html.span " "
                    yield Expr (depth + 1) x
                yield Html.span [ prop.text ")" ]
            ]
        ]
    | Lambda (parameters, body) ->
        List [
            Symbol "fn"
            List [ for p in parameters do Symbol p ]
            body
        ]
        |> Expr depth
    | _ ->
        Html.span [
            prop.className className
            prop.text (string expr)
        ]

[<ReactComponent>]
let EnvTable (env: Environment) onSymbolClick =
    Html.div [
        prop.className "env-table"
        prop.children [
            Html.table [
                prop.children [
                    Html.tbody [
                        for (symbol, expr) in env.ToArray() do
                            Html.tr [
                                Html.td [
                                    prop.title symbol
                                    prop.onClick (fun _ -> onSymbolClick symbol)
                                    prop.children [
                                        Html.div [
                                            prop.text (string symbol)
                                        ]
                                    ]
                                ]
                                Html.td [
                                    Expr 0 expr
                                ]
                            ]
                    ]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let HistoryBrowser history onItemClick =

    let ref = React.useRef None

    React.useEffect (fun () ->
        ref.current |> Option.iter (fun current ->
            let element = unbox<HTMLDivElement> current
            element.scrollTop <- element.scrollHeight))

    Html.div [
        prop.ref ref
        prop.className "history-browser"
        prop.children [
            for (input, expr, result) in history.Items do
                Html.dl [
                    Html.dt [
                        prop.onClick (fun _ -> onItemClick input)
                        prop.children [
                            match expr with
                            | Some expr -> Expr 0 expr
                            | None -> Html.span input
                        ]
                    ]
                    match result with
                    | Error message ->
                        Html.dd [
                            Html.span [
                                prop.className "error"
                                prop.text (string message)
                            ]
                        ]
                    | Ok expr ->
                        Html.dd [ Expr 0 expr ]
                ]
        ]
    ]

[<ReactComponent>]
let Repl (env: Environment) =
    let (input, setInput) = React.useState ""
    let (history, updateHistory) = React.useStateWithUpdater ({ Position = 0; Items = [||] })
    let inputRef = React.useRef None

    let update input =
        setInput ""
        match Parser.parse input with
        | Ok expr ->
            let result = env.Eval(expr)
            updateHistory (fun history -> history.Add(input, Some expr, result))
        | Error message ->
            updateHistory (fun history -> history.Add(input, None, Error message))

    React.useEffectOnce (fun () -> samples |> Array.iter update)

    React.useEffect (fun () ->
        inputRef.current |> Option.iter (fun current ->
            let element = unbox<HTMLInputElement> current
            element.focus()))

    let onInputKeyDown (e: KeyboardEvent) =
        let direction =
            match e.key with
            | "ArrowUp" -> -1
            | "ArrowDown" -> 1
            | _ -> 0
        if direction <> 0 then
            e.preventDefault() 
            let items = history.Items
            let position = max 0 (min (history.Position + direction) items.Length)
            if position < items.Length then
                let input, _, _ = items.[position]
                setInput input
            else
                setInput ""
            updateHistory (fun history -> { history with Position = position })

    React.fragment [
        EnvTable env setInput
        Html.form [
            prop.className "repl"
            prop.spellcheck false
            prop.onSubmit (fun e ->
                e.preventDefault()
                update input)
            prop.children [
                HistoryBrowser history setInput
                Html.input [
                    prop.ref inputRef
                    prop.type' "text"
                    prop.value input
                    prop.onChange setInput
                    prop.onKeyDown onInputKeyDown
                ]
            ]
        ]
    ]

let env = Environment.createDefaultEnvironment()

ReactDOM.render(Repl(env), document.getElementById "root")