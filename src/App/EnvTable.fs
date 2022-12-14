module FsLisp.App.EnvTable

open Feliz

open FsLisp.Lang
open FsLisp.App.SExprRenderer

[<ReactComponent>]
let EnvTable (env: IEnvironment) onSymbolClick =

    let getSemanticInfo = getSemanticInfoFromEnv env

    let getValueElement expr =
        match expr with
        | Builtin _ -> Html.span [ prop.className "comment"; prop.text ";built-in" ]
        | Nil -> Html.span [ prop.className "nil"; prop.text "nil" ]
        | List _ -> Html.span [ prop.className "comment"; prop.text ";list" ]
        | Lambda _ -> Html.span [ prop.className "comment"; prop.text ";lambda" ]
        | Symbol _ -> failwith "This should never happen."
        | Number _
        | Boolean _ -> SExprRenderer getSemanticInfo expr

    let isNotOperator (s: string) =
        s.ToCharArray() |> Array.exists System.Char.IsLetterOrDigit

    let children =
        [ for (symbol, expr) in env.ToArray() do
              if getSemanticInfo symbol <> SemanticInfo.Builtin || isNotOperator symbol then
                  Html.tr
                      [ Html.td
                            [ prop.className (getSemanticInfo symbol).ClassName
                              prop.onClick (fun _ -> onSymbolClick symbol)
                              prop.children [ Html.div [ prop.text (string symbol) ] ] ]
                        Html.td [ getValueElement expr ] ]
          for keyword in Keywords.asArray do
              Html.tr
                  [ Html.td
                        [ prop.className "keyword"
                          prop.onClick (fun _ -> onSymbolClick keyword)
                          prop.children [ Html.div [ prop.text keyword ] ] ]
                    Html.td [ Html.span [ prop.className "comment"; prop.text ";keyword" ] ] ] ]

    Html.div
        [ prop.className "env-table"
          prop.children [ Html.table [ Html.tbody children ] ] ]
