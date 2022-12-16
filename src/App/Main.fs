module FsLisp.App.Main

open Fable.Core.JsInterop
open Browser
open Browser.Types
open Feliz

open FsLisp.Lang
open FsLisp.App.Repl

let samples =
    [| "; Use def to create global variables/functions and let to bind values to names locally:"
       "(def (circle-area r) (let (pi 3.14) (* r r pi)))"
       "(def radii (range 1 5))"
       "; Calculate the areas of circles with an integer radius between 1 and 5 (exclusive):"
       "(map circle-area radii)" |]

let env: IEnvironment = Environment.createDefaultEnvironment ()

ReactDOM.render (Repl(env, samples), document.getElementById "root")

let fixHeight () =
    (document.querySelector ".repl" :?> HTMLElement)?style?height <- $"calc({window.innerHeight}px - 1rem)"

window.addEventListener ("resize", (fun _ -> fixHeight ()))

fixHeight ()
