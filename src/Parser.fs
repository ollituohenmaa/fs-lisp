module FsLisp.Parser

let private toOption<'a> (success, x: 'a) = if success then Some x else None

let private (|Int|_|) (s: string) = s |> System.Int32.TryParse |> toOption

let private (|Float|_|) (s: string) = s |> System.Double.TryParse |> toOption

let private (|Boolean|_|) s =
    match s with
    | "true" -> Some true
    | "false" -> Some false
    | _ -> None

let private tokenize (chars: string) =
    // wow
    // so lexer
    // many logic
    chars
        .Replace("(", " ( ")
        .Replace(")", " ) ")
        .Split([| ' '; '\t'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let rec private readList acc xs =
    match xs with
    | ")" :: tail -> acc |> List.rev |> List, tail
    | tokens ->
        let expr, tail = readSExpr tokens
        readList (expr :: acc) tail

and private readSExpr tokens =
    match tokens with
    | [] -> LispError.raise "Unexpected end of input."
    | ")" :: _ -> LispError.raise "Unexpected \")\"."
    | "(" :: tail -> readList [] tail
    | Int x :: tail -> Number(Int x), tail
    | Float x :: tail -> Number(Float x), tail
    | Boolean b :: tail -> Boolean b, tail
    | "nil" :: tail -> Nil, tail
    | s :: tail -> Symbol s, tail

let parse input =
    try
        match input |> tokenize |> readSExpr with
        | expr, [] -> Ok expr
        | _ -> Error "The input is not a single expression."
    with
    | LispError message -> Error message
    | exn ->
        printfn "%A" exn
        Error "Something went wrong."
