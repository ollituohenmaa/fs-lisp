module FsLisp.App.EnvTable

open Feliz

open FsLisp.Lang
open FsLisp.App.SExprRenderer

[<ReactComponent>]
let EnvTable (bindings: (string * SemanticInfo * SExpr)[]) =

    let getValueElement expr =
        match expr with
        | Builtin _ -> Html.span [ prop.className "comment"; prop.text ";built-in" ]
        | Nil -> Html.span [ prop.className "nil"; prop.text "nil" ]
        | List _ -> Html.span [ prop.className "comment"; prop.text ";list" ]
        | Lambda _ -> Html.span [ prop.className "comment"; prop.text ";lambda" ]
        | Symbol _ -> failwith "This should never happen."
        | Number _
        | Boolean _ -> SExprRenderer (fun _ -> SemanticInfo.Unknown) expr

    let isNotOperator (s: string) =
        s.ToCharArray() |> Array.exists System.Char.IsLetterOrDigit

    let children =
        [ for (symbol, semanticInfo, expr) in bindings do
              if semanticInfo <> SemanticInfo.Builtin || isNotOperator symbol then
                  Html.tr
                      [ Html.td
                            [ prop.className semanticInfo.ClassName
                              prop.children [ Html.div [ prop.text (string symbol) ] ] ]
                        Html.td [ getValueElement expr ] ]
          for keyword in Keywords.asArray do
              Html.tr
                  [ Html.td [ prop.className "keyword"; prop.children [ Html.div [ prop.text keyword ] ] ]
                    Html.td [ Html.span [ prop.className "comment"; prop.text ";keyword" ] ] ] ]

    Html.div
        [ prop.className "env-table"
          prop.children [ Html.table [ Html.tbody children ] ] ]
