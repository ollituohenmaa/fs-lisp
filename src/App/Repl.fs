module FsLisp.App.Repl

open Feliz

open FsLisp.App.History
open FsLisp.App.InputField

[<ReactComponent>]
let Repl (eval: string -> HistoryItem) (initialInput: string[]) =
    let (history, updateHistory) =
        React.useStateWithUpdater ({ Position = 0; Items = [||] })

    let inputValue, setInputValue = React.useState ""

    let handleInput input =
        setInputValue ""
        updateHistory (fun history -> history.Add(eval input))

    React.useEffectOnce (fun () -> initialInput |> Array.iter handleInput)

    let browseHistory key =
        let direction =
            match key with
            | "ArrowUp" -> -1
            | "ArrowDown" -> 1
            | _ -> 0

        let items = history.Items
        let position = max 0 (min (history.Position + direction) items.Length)

        if position < items.Length then
            let item = items.[position]

            let comment =
                match item.Comment with
                | Some comment -> " ;" + comment
                | None -> ""

            setInputValue (item.Code + comment)
        else
            setInputValue ""

        updateHistory (fun history -> { history with Position = position })

    Html.div
        [ prop.className "repl"
          prop.children [ History history.Items; InputField inputValue handleInput browseHistory ] ]
