namespace FsLisp.App

open FsLisp.Lang

[<RequireQualifiedAccess>]
type SemanticInfo =
    | Keyword
    | Lambda
    | Builtin
    | Variable
    | Unknown

    member this.ClassName =
        match this with
        | Keyword -> "keyword"
        | Lambda -> "lambda"
        | Builtin -> "builtin"
        | Variable -> "variable"
        | Unknown -> "unknown"

module SemanticInfo =

    let private fromSExpr expr =
        match expr with
        | Lambda _ -> SemanticInfo.Lambda
        | Builtin _ -> SemanticInfo.Builtin
        | _ -> SemanticInfo.Variable

    type IEnvironment with

        member this.GetSemanticInfo(name) =
            if Keywords.contains name then
                SemanticInfo.Keyword
            else
                try
                    fromSExpr (this.Find(name))
                with _ ->
                    SemanticInfo.Unknown
