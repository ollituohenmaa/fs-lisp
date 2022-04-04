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
let Repl(env: Environment) =
    let (input, setInput) = React.useState ""
    let (history, updateHistory) = React.useStateWithUpdater ({ Position = 0; Items = [||] })
    let historyRef = React.useRef None
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
                                        Expr 0 expr
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
                update input)
            prop.children [
                Html.div [
                    prop.ref historyRef
                    prop.className "history"
                    prop.children [
                        for (input, expr, result) in history.Items do
                            Html.dl [
                                Html.dt [
                                    prop.onClick (fun _ -> setInput input)
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
                Html.input [
                    prop.ref inputRef
                    prop.type' "text"
                    prop.value input
                    prop.onChange setInput
                    prop.onKeyDown (fun e ->
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
                            updateHistory (fun history -> { history with Position = position }))
                ]
            ]
        ]
    ]

let env = Environment.createDefaultEnvironment()

ReactDOM.render(Repl(env), document.getElementById "root")