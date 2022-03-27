module FsLisp.App

open Browser
open Browser.Types
open Feliz

let fib = "(def fib (fn (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))"

[<ReactComponent>]
let Expr(expr: SExpr) =
    match expr with
    | Builtin _
    | List [] ->
        Html.span [
            prop.className "uninteresting"
            prop.text (string expr)
        ]
    | List _
    | Lambda _ ->
        Html.span [
            prop.className "list"
            prop.text (string expr)
        ]
    | _ -> Html.dd (string expr)

[<ReactComponent>]
let Repl(env: Environment) =
    let (input, setInput) = React.useState("(fib 10)")
    let (history, updateHistory) = React.useStateWithUpdater([||])
    let historyRef = React.useRef(None)
    let inputRef = React.useRef(None)

    let update input =
        match Parser.parse input with
        | Ok expr ->
            let result = env.Eval(expr)
            updateHistory (fun xs -> [| yield! xs; (input, result) |])
        | Error message ->
            updateHistory (fun xs -> [| yield! xs; (input, Error message) |])

    React.useEffectOnce (fun () -> update fib)

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
                                        prop.className "symbol"
                                        prop.onClick (fun _ -> setInput symbol)
                                        prop.text symbol
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
                        for (input, result) in history do
                            Html.dl [
                                Html.dt [
                                    prop.onClick (fun _ -> setInput input)
                                    prop.text input
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