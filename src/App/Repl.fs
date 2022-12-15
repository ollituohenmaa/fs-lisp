module FsLisp.App.Repl

open Browser.Types
open Feliz

open FsLisp.Lang
open FsLisp.App.EnvTable
open FsLisp.App.History

[<ReactComponent>]
let Repl (env: IEnvironment, initialInput: string[]) =
    let (input, updateInput) = React.useStateWithUpdater ""

    let setInput input = updateInput (fun _ -> input)

    let (history, updateHistory) =
        React.useStateWithUpdater ({ Position = 0; Items = [||] })

    let inputRef = React.useRef None

    let update input =
        setInput ""

        match Parser.parse input with
        | Ok expr, code, comment ->
            updateHistory (fun history ->
                history.Add(
                    { Expr = Some expr
                      Result = env.Eval(expr)
                      Env = env.Copy()
                      Code = code
                      Comment = comment }
                ))
        | Error message, code, comment ->
            updateHistory (fun history ->
                history.Add(
                    { Expr = None
                      Result = Error message
                      Env = env.Copy()
                      Code = code
                      Comment = comment }
                ))

    React.useEffectOnce (fun () -> initialInput |> Array.iter update)

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
                let item = items.[position]

                let comment =
                    match item.Comment with
                    | Some comment -> " ;" + comment
                    | None -> ""

                setInput (item.Code + comment)
            else
                setInput ""

            updateHistory (fun history -> { history with Position = position })

    let insertAtCursor text =
        let selectionStart, selectionEnd =
            match inputRef.current with
            | Some current ->
                let element = unbox<HTMLInputElement> current
                element.selectionStart, element.selectionEnd
            | None -> 0, 0

        updateInput (fun input -> input.Substring(0, selectionStart) + text + input.Substring(selectionEnd))

    React.fragment
        [ EnvTable env insertAtCursor
          Html.form
              [ prop.className "repl"
                prop.spellcheck false
                prop.onSubmit (fun e ->
                    e.preventDefault ()
                    update input)
                prop.children
                    [ History history.Items
                      Html.input
                          [ prop.ref inputRef
                            prop.type' "text"
                            prop.value input
                            prop.onChange setInput
                            prop.onKeyDown onInputKeyDown ] ] ] ]
