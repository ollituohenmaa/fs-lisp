namespace FsLisp

open System

[<CustomEquality; CustomComparison>]
type Number =
    | Int of int
    | Float of float

    static member private operator fnInt fnFloat x y =
        match x, y with
        | Int x, Int y -> fnInt x y |> Int
        | Int x, Float y -> fnFloat (float x) y |> Float
        | Float x, Int y -> fnFloat x (float y) |> Float
        | Float x, Float y -> fnFloat x y |> Float

    static member (+)(x, y) = Number.operator (+) (+) x y
    static member (-)(x, y) = Number.operator (-) (-) x y
    static member (*)(x, y) = Number.operator (*) (*) x y
    static member (/)(x, y) = Number.operator (/) (/) x y
    static member (%)(x, y) = Number.operator (%) (%) x y

    override this.ToString() =
        match this with
        | Int x -> string x
        | Float x -> string x

    interface IEquatable<Number> with

        member this.Equals(other) =
            match this, other with
            | Int x, Int y -> x = y
            | Int x, Float y -> (float x) = y
            | Float x, Int y -> x = (float y)
            | Float x, Float y -> x = y

    override this.Equals(other) =
        match other with
        | :? Number as other -> (this :> IEquatable<_>).Equals(other)
        | _ -> false

    override _.GetHashCode() = raise (NotImplementedException())

    interface IComparable<Number> with

        member this.CompareTo(other) =
            let isSmallerThanOther =
                match this, other with
                | Int x, Int y -> x < y
                | Int x, Float y -> (float x) < y
                | Float x, Int y -> x < (float y)
                | Float x, Float y -> x < y

            if isSmallerThanOther then -1
            elif this = other then 0
            else 1

    interface IComparable with

        member this.CompareTo(other) =
            match other with
            | :? Number as other -> (this :> IComparable<_>).CompareTo(other)
            | _ -> -1
