open System.IO

open FSharpPlus
open FSharpPlus.Data

type Cell = Open | Tree

type Row = Row of Cell[]

type Grid = Grid of Row[]

type Coordinates = {
    X : int
    Y : int
}

let parseCell = function '.' -> Some Open | '#' -> Some Tree | _ -> None

let parseRow = Seq.map parseCell >> sequence >> Option.map Array.ofSeq >> Option.map Row

let getInput file =
    file
    |> File.ReadAllLines
    |> Array.map parseRow
    |> sequence
    |> Option.map Grid

let initCoordinates = {
    X = 0
    Y = 0
}

let moveOnce (Grid rows) x y = monad {
    let! currentState = State.get
    let newY = currentState.Y + y

    if newY >= rows.Length then
        None
    else
        let (Row cells) = rows.[newY]
        let newX = (currentState.X + x) % cells.Length
        do! State.put { X = newX; Y = newY }
        Some cells.[newX]
}

let someIfFirstSome (a, b) =
    match a with
    | Some a -> Some (a, b)
    | None -> None

let moveFull grid x y =
    List.unfold (State.run (moveOnce grid x y) >> someIfFirstSome) initCoordinates
    |> List.filter ((=) Tree)
    |> List.length

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        match getInput file with
        | Some grid ->
            let moveFull = moveFull grid

            moveFull 3 1 |> printfn "%d"

            [
                moveFull 1 1
                moveFull 3 1
                moveFull 5 1
                moveFull 7 1
                moveFull 1 2
            ]
            |> List.map bigint
            |> List.fold (*) (bigint 1)
            |> printfn "%O"

            0
        | None ->
            printfn "Invalid input"
            2
    | _ ->
        printfn "Usage: Aoc.Day2 file"
        1
