module FsLisp.App.InputField

open System
open Browser.Types
open Feliz

[<ReactComponent>]
let InputField (initialValue: string) (onEnterDown: string -> unit) (onArrowKeyDown: string -> unit) =
    let value, setValue = React.useState initialValue
    let inputRef = React.useRef None

    React.useEffect (fun () ->
        inputRef.current
        |> Option.iter (fun current ->
            let element = unbox<HTMLInputElement> current
            element.focus ()))

    React.useEffect ((fun () -> setValue initialValue), [| box initialValue |])

    let onInputKeyDown (e: KeyboardEvent) =
        match e.key with
        | "Enter" ->
            e.preventDefault ()

            if not (String.IsNullOrWhiteSpace(value)) then
                onEnterDown value

            setValue ""
        | "ArrowUp"
        | "ArrowDown" ->
            e.preventDefault ()
            onArrowKeyDown e.key
        | _ -> ()

    Html.input
        [ prop.ref inputRef
          prop.type' "text"
          prop.value value
          prop.onChange setValue
          prop.onKeyDown onInputKeyDown ]
