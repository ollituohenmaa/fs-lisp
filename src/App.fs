module FsLisp.App

open Browser
open Browser.Types
open Feliz

let samples =
    [| "(def pi 3.14)"
       "(def circle-area (fn (r) (* pi (* r r))))"
       "(map circle-area (range 1 5))" |]

let keywords =
    Set([ Keyword.Definition
          Keyword.Lambda
          Keyword.Conditional
          Keyword.Quote ])

[<ReactComponent>]
let rec Expr(expr: SExpr) =

    let className =
        match expr with
        | Builtin _ -> "builtin"
        | Nil -> "nil"
        | List _ -> "list"
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
                yield Expr(head)
                for x in tail do
                    yield Html.span " "
                    yield Expr(x)
                yield Html.span [ prop.text ")" ]
            ]
        ]
    | Lambda (parameters, body) ->
        List [
            Symbol "fn"
            List [ for p in parameters do Symbol p ]
            body
        ]
        |> Expr
    | _ ->
        Html.span [
            prop.className className
            prop.text (string expr)
        ]

[<ReactComponent>]
let Repl(env: Environment) =
    let (input, setInput) = React.useState ""
    let (history, updateHistory) = React.useStateWithUpdater [||]
    let historyRef = React.useRef None
    let inputRef = React.useRef None

    let update input =
        match Parser.parse input with
        | Ok expr ->
            let result = env.Eval(expr)
            updateHistory (fun xs -> [| yield! xs; (input, Some expr, result) |])
        | Error message ->
            updateHistory (fun xs -> [| yield! xs; (input, None, Error message) |])

    React.useEffectOnce (fun () -> samples |> Array.iter update)

    React.useEffect(fun () ->
        historyRef.current |> Option.iter (fun current ->
            let element = unbox<HTMLDivElement> current
            element.scrollTop <- element.scrollHeight)
        inputRef.current |> Option.iter (fun current ->
            let element = unbox<HTMLInputElement> current
            element.focus()))

    React.fragment [
        Html.div [
            prop.className "env"
            prop.children [
                Html.table [
                    prop.children [
                        Html.tbody [
                            for (symbol, expr) in env.ToArray() do
                                Html.tr [
                                    Html.td [
                                        prop.title symbol
                                        prop.onClick (fun _ -> setInput symbol)
                                        prop.children [
                                            Html.div [
                                                prop.text (string symbol)
                                            ]
                                        ]
                                    ]
                                    Html.td [
                                        Expr(expr)
                                    ]
                                ]
                        ]
                    ]
                ]
            ]
        ]
        Html.form [
            prop.className "repl"
            prop.spellcheck false
            prop.onSubmit (fun e ->
                e.preventDefault()
                update input
                setInput "")
            prop.children [
                Html.div [
                    prop.ref historyRef
                    prop.className "history"
                    prop.children [
                        for (input, expr, result) in history do
                            Html.dl [
                                Html.dt [
                                    prop.onClick (fun _ -> setInput input)
                                    prop.children [
                                        match expr with
                                        | Some expr -> Expr(expr)
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
                                    Html.dd [ Expr(expr) ]
                            ]
                    ]
                ]
                Html.input [
                    prop.ref inputRef
                    prop.type' "text"
                    prop.value input
                    prop.onChange setInput
                ]
            ]
        ]
    ]

let env = Environment.createDefaultEnvironment()

ReactDOM.render(Repl(env), document.getElementById "root")