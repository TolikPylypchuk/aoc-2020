open System
open System.IO

open FSharpPlus
open FSharpPlus.Data

type ContiguousSetState = {
    Start : int
    End : int
    CurrentSum : int64
}

let parseToInt64 (str : string) =
    match Int64.TryParse(str) with
    | true, num -> Some num
    | _ -> None

let getInput file =
    file
    |> File.ReadLines
    |> Seq.choose parseToInt64
    |> Array.ofSeq

let isNumberValid =
    function
    | numToCheck :: nums ->
        nums
        |> List.allPairs nums
        |> List.filter (fun (a, b) -> a <> b)
        |> List.tryFind (fun (a, b) -> a + b = numToCheck)
        |> Option.isSome
    | [] -> false

let rec tryFindContiguousSetFirstLastSum (nums : int64[]) sum = monad {
    let! state = State.get
    if state.End = nums.Length then
        return None
    else
        let nextNum = nums.[state.End]
        let currentSum = state.CurrentSum + nextNum

        if currentSum = sum then
            let slice = nums.[state.Start .. state.End]
            let min = slice |> Array.min
            let max = slice |> Array.max
            return Some <| min + max
        elif currentSum < sum then
            do! State.put { state with End = state.End + 1; CurrentSum = currentSum }
            return! tryFindContiguousSetFirstLastSum nums sum
        else
            do! State.put { Start = state.Start + 1; End = state.Start + 1; CurrentSum = 0L }
            return! tryFindContiguousSetFirstLastSum nums sum
}

let initialState = { Start = 0; End = 0; CurrentSum = 0L }

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let allNums =
            file
            |> getInput
            |> Array.ofSeq

        let maybeValidNumber =
            allNums
            |> List.ofArray
            |> List.windowed 26
            |> List.map List.rev
            |> List.skipWhile isNumberValid
            |> List.tryHead
            |> Option.bind List.tryHead

        maybeValidNumber |> Option.iter (printfn "%d")

        monad {
            let! validNumber = maybeValidNumber
            let tryFind = validNumber |> tryFindContiguousSetFirstLastSum allNums
            let! sum = State.eval tryFind initialState
            do printfn "%d" sum
        } |> ignore

        0
    | _ ->
        printfn "Usage: Aoc.Day9 file"
        1
