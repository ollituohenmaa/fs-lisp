module FsLisp.App.EnvTable

open Feliz

open FsLisp.Lang

[<ReactComponent>]
let EnvTable (getSemanticInfo: string -> SemanticInfo) (bindings: (string * SExpr)[]) =

    let getValueElement expr =
        let className, text =
            match expr with
            | Builtin _ -> "comment", ";built-in"
            | List _ -> "comment", ";list"
            | Lambda _ -> "comment", ";lambda"
            | Nil -> "nil", string expr
            | Number _ -> "number", string expr
            | Boolean _ -> "boolean", string expr
            | Symbol s -> (getSemanticInfo s).ClassName, string expr

        Html.span [ prop.className className; prop.text text ]

    let isNotOperator (s: string) =
        s.ToCharArray() |> Array.exists System.Char.IsLetterOrDigit

    let children =
        [ for (symbol, expr) in bindings do
              let semanticInfo = getSemanticInfo symbol

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
