open System.IO

open FSharpPlus

type Cell =
    | Floor
    | EmptySeat
    | OccupiedSeat

let parseCell =
    function
    | '.' -> Some Floor
    | 'L' -> Some EmptySeat
    | '#' -> Some OccupiedSeat
    | _ -> None

let getInput =
    File.ReadLines
    >> Seq.map (Seq.map parseCell >> sequence)
    >> sequence
    >> Option.map array2D

let getAdjacentCellsSimple (row, col) (cells : 'a[,]) =
    let getCellsInRow row' = seq {
        if col > 0 then
            yield cells.[row', col - 1]

        if row' <> row then 
            yield cells.[row', col]

        if col + 1 < Array2D.length2 cells then
            yield cells.[row', col + 1]
    }

    seq {
        if row > 0 then
            yield! getCellsInRow <| row - 1

        yield! getCellsInRow row

        if row + 1 < Array2D.length1 cells then
            yield! getCellsInRow <| row + 1
    }

let getAdjacentCellsExtended (row, col) (cells : Cell[,]) =
    let numRows = Array2D.length1 cells
    let numCols = Array2D.length2 cells

    let rec getNextCell (row, col) getNextRow getNextCol =
        let nextRow = getNextRow row
        let nextCol = getNextCol col

        if nextRow < 0 || nextRow >= numRows || nextCol < 0 || nextCol >= numCols then
            Floor
        else
            match cells.[nextRow, nextCol] with
            | OccupiedSeat -> OccupiedSeat
            | EmptySeat -> EmptySeat
            | Floor -> getNextCell (nextRow, nextCol) getNextRow getNextCol

    let add = (+) 1
    let subtract = (flip (-)) 1

    seq {
        yield getNextCell (row, col) subtract subtract
        yield getNextCell (row, col) subtract id
        yield getNextCell (row, col) subtract add

        yield getNextCell (row, col) id subtract
        yield getNextCell (row, col) id add

        yield getNextCell (row, col) add subtract
        yield getNextCell (row, col) add id
        yield getNextCell (row, col) add add
    }

let isOccupied = function OccupiedSeat -> true | _ -> false

let boolToInt = function true -> 1 | false -> 0

let calculateNextCellState (row, col) max getAdjacentCells (cells : Cell[,]) =
    match cells.[row, col] with
    | EmptySeat when cells |> getAdjacentCells (row, col) |> Seq.filter isOccupied |> Seq.length = 0 -> OccupiedSeat
    | OccupiedSeat when cells |> getAdjacentCells (row, col) |> Seq.filter isOccupied |> Seq.length > max -> EmptySeat
    | cell -> cell

let calculateNextState max getAdjacentCells cells =
    cells |> Array2D.mapi (fun row col _ -> cells |> calculateNextCellState (row, col) max getAdjacentCells)

let cellsEqual a b =
    Seq.zip (a |> Seq.cast<Cell>) (b |> Seq.cast<Cell>)
    |> Seq.map (fun (a, b) -> a = b)
    |> Seq.fold (&&) true

let rec calculateOccupiedSeats max getAdjacentCells cells =
    let nextCells = cells |> calculateNextState max getAdjacentCells
    if cells |> cellsEqual nextCells then
        cells |> Array2D.map (isOccupied >> boolToInt) |> Seq.cast<int> |> Seq.fold (+) 0
    else
        nextCells |> calculateOccupiedSeats max getAdjacentCells

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        monad {
            let! input = file |> getInput
            let result1 = input |> calculateOccupiedSeats 3 getAdjacentCellsSimple
            do printfn "%d" result1
            
            let result2 = input |> calculateOccupiedSeats 4 getAdjacentCellsExtended
            do printfn "%d" result2
        } |> ignore
        
        0
    | _ ->
        printfn "Usage: Aoc.Day11 file"
        1
