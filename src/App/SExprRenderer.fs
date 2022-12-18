module FsLisp.App.SExprRenderer

open Feliz

open FsLisp.Lang
open FsLisp.App.SemanticInfo

[<ReactComponent>]
let rec SExprRenderer (getSemanticInfo: string -> SemanticInfo) (expr: SExpr) =
    match expr with
    | List(head :: tail) ->
        let parameters =
            match head :: tail with
            | Symbol Keywords.Lambda :: List(parameters) :: _
            | Symbol Keywords.Definition :: List(Symbol _ :: parameters) :: _
            | Symbol Keywords.Let :: List [ List(Symbol _ :: parameters); _ ] :: _ ->
                try
                    parameters |> List.map SExpr.toSymbol |> Set.ofList
                with :? LispError ->
                    Set.empty
            | Symbol Keywords.Definition :: Symbol name :: _
            | Symbol Keywords.Let :: List [ Symbol name; _ ] :: _ -> Set.singleton name
            | _ -> Set.empty

        let lambdaName =
            match head :: tail with
            | Symbol Keywords.Definition :: List(Symbol name :: _) :: _ -> Some name
            | Symbol Keywords.Let :: List [ List(Symbol name :: _); _ ] :: _ -> Some name
            | _ -> None

        let getSemanticInfo s =
            if Some s = lambdaName then SemanticInfo.Lambda
            elif parameters.Contains(s) then SemanticInfo.Variable
            else getSemanticInfo s

        Html.span
            [ prop.className "list"
              prop.children
                  [ yield Html.span [ prop.text "(" ]
                    yield SExprRenderer getSemanticInfo head
                    for x in tail do
                        yield Html.span " "
                        yield SExprRenderer getSemanticInfo x
                    yield Html.span [ prop.text ")" ] ] ]
    | Lambda(lambdaEnv, parameters, body) ->
        List
            [ Symbol Keywords.Lambda
              List
                  [ for p in parameters do
                        Symbol p ]
              body ]
        |> SExprRenderer lambdaEnv.GetSemanticInfo
    | Builtin _ -> Html.span [ prop.className "comment"; prop.text ";built-in" ]
    | _ ->
        let className =
            match expr with
            | Builtin _
            | Lambda _ -> failwith "This should never happen."
            | Nil _ -> "nil"
            | List _ -> "list"
            | Number _ -> "number"
            | Boolean _ -> "boolean"
            | Symbol s -> (getSemanticInfo s).ClassName

        Html.span [ prop.className className; prop.text (string expr) ]
