open System
open System.IO

let getInput =
    File.ReadAllLines
    >> function [| cups |] -> cups |> Seq.map (Char.GetNumericValue >> int) |> List.ofSeq | _ -> []

let createCupsMap cups =
    cups
    |> List.pairwise
    |> Map.ofList
    |> Map.add (cups |> List.last) (cups |> List.head)

let moveOnce (minCup, maxCup) (cups, currentCup) =
    let cup1 = cups |> Map.find currentCup
    let cup2 = cups |> Map.find cup1
    let cup3 = cups |> Map.find cup2

    let newNextCup = cups |> Map.find cup3

    let rec findNextCup currentCup =
        let nextCup = if currentCup = minCup then maxCup else currentCup - 1
        if cup1 <> nextCup && cup2 <> nextCup && cup3 <> nextCup
        then nextCup
        else nextCup |> findNextCup

    let nextCup = currentCup |> findNextCup
    let nextNextCup = cups |> Map.find nextCup

    let newCups =
        cups
        |> Map.add currentCup newNextCup
        |> Map.add nextCup cup1
        |> Map.add cup3 nextNextCup

    newCups, newNextCup

let rec move num (min, max) (cups, firstCup) =
    if num = 0 then cups
    else moveOnce (min, max) (cups, firstCup) |> move (num - 1) (min, max)

let rec cupsAfter cup cups =
    Seq.unfold (fun currentCup ->
        if currentCup <> cup
        then let nextCup = cups |> Map.find currentCup in Some (currentCup, nextCup)
        else None)
        (cups |> Map.find cup)

let addMoreCups max cups =
    cups @ [ (cups |> List.max) + 1 .. max]

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = file |> getInput
        let minCup = input |> List.min
        let firstCup = input |> List.head

        input
        |> createCupsMap
        |> fun cups -> move 100 (minCup, input |> List.max) (cups, firstCup)
        |> cupsAfter 1
        |> Seq.map string
        |> Seq.fold (+) ""
        |> printfn "%s"

        let maxCup = 1000000

        input
        |> addMoreCups maxCup
        |> createCupsMap
        |> fun cups -> move 10000000 (minCup, maxCup) (cups, firstCup)
        |> cupsAfter 1
        |> Seq.take 2
        |> Seq.map int64
        |> Seq.fold (*) 1L
        |> printfn "%d"
        0
    | _ ->
        printfn "Usage: Aoc.Day23 file"
        1
