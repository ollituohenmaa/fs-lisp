namespace FsLisp.Lang

type Environment(?symbols: Map<string, SExpr>, ?parent: IEnvironment) =

    let mutable symbols = symbols |> Option.defaultValue Map.empty

    let exprOrdering expr =
        match expr with
        | List _ -> 0
        | Symbol _ -> 1
        | Boolean _ -> 2
        | Number _ -> 3
        | Nil -> 4
        | Lambda _ -> 5
        | Builtin _ -> 6

    interface IEnvironment with

        member _.Add(symbol, expr) = symbols <- symbols.Add(symbol, expr)

        member _.AddToRoot(symbol, expr) =
            match parent with
            | Some parent -> parent.AddToRoot(symbol, expr)
            | None -> symbols <- symbols.Add(symbol, expr)

        member _.Find(s) =
            match symbols.TryFind(s), parent with
            | Some value, _ -> value
            | None, Some parent -> parent.Find(s)
            | None, _ -> LispError.raise $"Symbol {s} is undefined."

        member this.CreateChild(?bindings) =
            Environment(bindings |> Option.defaultValue [] |> Map.ofList, this) :> IEnvironment

        member _.Copy() =
            match parent with
            | Some parent -> Environment(symbols, parent.Copy()) :> IEnvironment
            | None -> Environment(symbols) :> IEnvironment

        member _.ToArray() =
            [| yield! symbols |> Map.toArray
               match parent with
               | Some parent -> yield! parent.ToArray()
               | None -> () |]
            |> Array.distinctBy fst
            |> Array.sortBy (fun (s, e) -> (exprOrdering e, s))

        member this.Eval(expr) =
            try
                SExpr.eval this expr |> Ok
            with
            | LispError s -> Error s
            | exn when exn.Message = "Maximum call stack size exceeded" -> Error "Stack overflow (no, not the website)."
            | exn ->
                printfn "%A" exn
                Error "Something went wrong."

module Environment =

    let createDefaultEnvironment () =
        let env = Environment(StdLib.builtins) :> IEnvironment
        StdLib.lambdas |> List.iter (env.Eval >> ignore)
        env
