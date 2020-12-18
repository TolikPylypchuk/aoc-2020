open System
open System.IO
open System.Text.RegularExpressions

type Seat = {
    Row : int
    Column : int
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let binaryToInt str = Convert.ToInt32(str, 2)

let flip f a b = f b a

let parseSeat =
    function
    | Regex "^((?:F|B){7})((?:L|R){3})$" [ row; column ] ->
        let rowNum =
            row
            |> String.map (function 'F' -> '0' | _ -> '1')
            |> binaryToInt

        let columnNum =
            column
            |> String.map (function 'L' -> '0' | _ -> '1')
            |> binaryToInt

        Some { Row = rowNum; Column = columnNum }
    | _ -> None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.choose parseSeat

let seatId seat = seat.Row * 8 + seat.Column

let mySeatId seatIds =
    let maxId = seatIds |> List.fold max 0

    let absentIds =
        [ 0 .. maxId ]
        |> List.filter (not << (flip List.contains <| seatIds))

    absentIds
    |> List.filter (fun id -> (not <| List.contains (id + 1) absentIds) && (not <| List.contains (id - 1) absentIds))
    |> List.tryHead

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let seatIds =
            file
            |> getInput
            |> List.map seatId

        let maxId = seatIds |> List.fold max 0
        printfn "%d" maxId

        seatIds
        |> mySeatId
        |> Option.iter (printfn "%d")
        0
    | _ ->
        printfn "Usage: Aoc.Day5 file"
        1
