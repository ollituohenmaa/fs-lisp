namespace FsLisp

type Environment(symbols: Map<string, SExpr>, ?parent: IEnvironment) =

    let mutable symbols = symbols

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

        member _.Find(s) =
            match symbols.TryFind(s), parent with
            | Some value, _ -> value
            | None, Some parent -> parent.Find(s)
            | None, _ -> LispError.raise $"Symbol {s} is undefined."

        member this.CreateFunctionEnvironment(parameters: _ list, arguments: _ list) =
            Environment((parameters, arguments) ||> List.zip |> Map.ofList, this) :> IEnvironment

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

    let private builtins =
        [ "+", SExpr.add
          "-", SExpr.sub
          "*", SExpr.mul
          "/", SExpr.div
          "%", SExpr.rem
          ">", SExpr.gt
          ">=", SExpr.ge
          "<", SExpr.lt
          "<=", SExpr.le
          "=", List.pairwise >> List.forall (fun (x, y) -> x = y) >> Boolean
          "<>", List.pairwise >> List.exists (fun (x, y) -> x <> y) >> Boolean
          "list", List
          "cons", SExpr.cons
          "head", SExpr.head
          "tail", SExpr.tail ]
        |> List.map (fun (s, v) -> (s, Builtin v))
        |> Map.ofList

    let private lambdas =
        [ "(def (not bool) (if bool false true))"
          "(def (abs x) (if (>= x 0) x (* x -1)))"
          "(def (fold f acc xs) (if (= xs (list)) acc (fold f (f acc (head xs)) (tail xs))))"
          "(def (reverse xs) (fold (fn (acc x) (cons x acc)) () xs))"
          "(def (count xs) (fold (fn (acc x) (+ acc 1)) 0 xs))"
          "(def (map f xs) (reverse (fold (fn (acc x) (cons (f x) acc)) () xs)))"
          "(def (filter f xs) (reverse (fold (fn (acc x) (if (f x) (cons x acc) acc)) () xs)))"
          "(def (range start stop) (if (>= start stop) () (cons start (range (+ start 1) stop))))" ]
        |> List.choose (fun x ->
            match Parser.parse x with
            | Ok expr -> Some expr
            | _ -> None)

    let createDefaultEnvironment () =
        let env = Environment(builtins) :> IEnvironment
        lambdas |> List.iter (env.Eval >> ignore)
        env
