open System
open System.IO

type GameState = {
    CurrentTurn : int
    PreviousNumber : int
    Numbers : Map<int, int>
}

let stringToInt (str : string) =
    match str |> Int32.TryParse with
    | true, num -> Some num
    | _ -> None

let stringToInts (str : string) =
    str.Split(',')
    |> List.ofSeq
    |> List.choose stringToInt

let getInput file =
    match file |> File.ReadAllLines with
    | [| nums |] -> Some nums
    | _ -> None

let initialState = { CurrentTurn = 1; PreviousNumber = -1; Numbers = Map.empty }

let playInitial state num =
    {
        CurrentTurn = state.CurrentTurn + 1
        PreviousNumber = num
        Numbers = state.Numbers |> Map.add num state.CurrentTurn
    }

let playOnce state =
    let previousTurn = state.CurrentTurn - 1

    let lastSeenTurn = state.Numbers |> Map.tryFind state.PreviousNumber |> Option.defaultValue previousTurn
    let newNums = state.Numbers |> Map.add state.PreviousNumber previousTurn

    let newState = {
        CurrentTurn = state.CurrentTurn + 1
        PreviousNumber = previousTurn - lastSeenTurn
        Numbers = newNums
    }

    Some (newState, newState)

let playGame numTurns =
    Seq.fold playInitial initialState
    >> Seq.unfold playOnce
    >> Seq.skipWhile (fun state -> state.CurrentTurn <= numTurns)
    >> Seq.head
    >> fun state -> state.PreviousNumber

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        file
        |> getInput
        |> Option.map stringToInts
        |> Option.map (playGame 2020)
        |> Option.iter (printfn "%d")
        
        file
        |> getInput
        |> Option.map stringToInts
        |> Option.map (playGame 30000000)
        |> Option.iter (printfn "%d")

        0
    | _ ->
        printfn "Usage: Aoc.Day15 file"
        1
