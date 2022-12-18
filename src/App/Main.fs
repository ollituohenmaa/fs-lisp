module FsLisp.App.Main

open Fable.Core.JsInterop
open Browser
open Browser.Types
open Feliz

open FsLisp.Lang
open FsLisp.App.SemanticInfo
open FsLisp.App.EnvTable
open FsLisp.App.History
open FsLisp.App.Repl

[<ReactComponent>]
let Root (env: IEnvironment) (samples: string[]) =
    let (bindings, setBindings) = React.useState (env.ToArrayWithSemanticInfo())

    let eval input =
        match Parser.parse input with
        | Ok expr, code, comment ->
            let result = env.Eval(expr)
            setBindings (env.ToArrayWithSemanticInfo())

            { Expr = Some expr
              Result = result
              GetSemanticInfo = env.Copy().GetSemanticInfo
              Code = code
              Comment = comment }
        | Error message, code, comment ->
            { Expr = None
              Result = Error message
              GetSemanticInfo = env.Copy().GetSemanticInfo
              Code = code
              Comment = comment }

    React.fragment [ EnvTable bindings; Repl eval samples ]

let samples =
    [| "; Use def to create global variables/functions and let to bind values to names locally:"
       "(def (circle-area r) (let (pi 3.14) (* r r pi)))"
       "(def radii (range 1 5))"
       "; Calculate the areas of circles with an integer radius between 1 and 5 (exclusive):"
       "(map circle-area radii)" |]

let env = Environment.createDefaultEnvironment ()

ReactDOM.render (Root env samples, document.getElementById "root")

let fixHeight () =
    (document.querySelector ".repl" :?> HTMLElement)?style?height <- $"calc({window.innerHeight}px - 1rem)"

window.addEventListener ("resize", (fun _ -> fixHeight ()))

fixHeight ()
