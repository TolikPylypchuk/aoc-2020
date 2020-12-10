open System
open System.IO

type CalculationState = {
    Cache : Map<int list, int64>
    List : int list
}

let strToInt (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> List.choose strToInt
    >> List.sort
    >> fun nums -> 0 :: nums
    >> fun nums -> nums @ [ List.max nums + 3 ]
    
let calculateDifferences =
    List.pairwise
    >> List.map (fun (a, b) -> b - a)
    >> List.groupBy id
    >> List.map (fun (num, nums) -> num, nums |> List.length)
    >> Map.ofList

let tryFind key = Map.tryFind key >> Option.defaultValue 0
let tryFind' key = Map.tryFind key >> Option.defaultValue 0L

let combinations num state =
    let rec combinationsForSublist list acc =
        match list with
        | [] -> 1L
        | nextNum :: otherNums ->
            if nextNum - num <= 3 then
                combinationsForSublist otherNums <| acc + (state.Cache |> tryFind' list)
            else
                acc

    let numCombinations = combinationsForSublist state.List 0L

    let newList = num :: state.List
    { Cache = state.Cache |> Map.add newList numCombinations; List = newList }

let numberOfCombinations nums =
    List.foldBack combinations nums { Cache = Map.empty; List = [] }
    |> fun state -> state.Cache |> tryFind' nums

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let nums = file |> getInput
        let differences = nums |> calculateDifferences

        let ones = differences |> tryFind 1
        let threes = differences |> tryFind 3

        printfn "%d" (ones * threes)

        printfn "%d" (nums |> numberOfCombinations)

        0
    | _ ->
        printfn "Usage: Aoc.Day10 file"
        1
