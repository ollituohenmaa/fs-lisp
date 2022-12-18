module FsLisp.App.History

open Browser.Types
open Feliz

open FsLisp.Lang
open FsLisp.App.SExprRenderer

type HistoryItem =
    { Expr: SExpr option
      Result: Result<SExpr, string>
      GetSemanticInfo: string -> SemanticInfo
      Code: string
      Comment: string option }

type HistoryModel =
    { Position: int
      Items: HistoryItem[] }

    member this.Add(item) =
        { Position = this.Items.Length + 1
          Items = [| yield! this.Items; yield item |] }

[<ReactComponent>]
let History items =

    let ref = React.useRef None

    React.useEffect (fun () ->
        ref.current
        |> Option.iter (fun current ->
            let element = unbox<HTMLDivElement> current
            element.scrollTop <- element.scrollHeight))

    let children =
        [ for item in items do
              let comment =
                  match item.Comment with
                  | Some comment -> ";" + comment
                  | None -> ""

              Html.dl
                  [ if comment <> "" || not (System.String.IsNullOrWhiteSpace(item.Code)) then
                        Html.dt
                            [ prop.children
                                  [ match item.Expr with
                                    | _ when System.String.IsNullOrWhiteSpace(item.Code) ->
                                        Html.span [ prop.className "comment"; prop.text comment ]
                                    | Some expr ->
                                        React.fragment
                                            [ SExprRenderer item.GetSemanticInfo expr
                                              Html.span [ prop.className "comment"; prop.text comment ] ]
                                    | None ->
                                        React.fragment
                                            [ Html.span item.Code
                                              Html.span [ prop.className "comment"; prop.text comment ] ] ] ]
                    if not (System.String.IsNullOrWhiteSpace(item.Code)) then
                        Html.dd
                            [ match item.Result with
                              | Ok _ -> prop.className "result"
                              | _ -> ()
                              prop.children
                                  [ match item.Result with
                                    | Error message -> Html.span [ prop.className "error"; prop.text (string message) ]
                                    | Ok expr -> SExprRenderer item.GetSemanticInfo expr ] ] ] ]

    Html.div [ prop.ref ref; prop.className "history"; prop.children children ]
