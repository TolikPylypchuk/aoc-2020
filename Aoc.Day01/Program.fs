open System
open System.IO

let strToInt (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.map strToInt
    |> List.choose id

let calculateExpenses1 input =
    input
    |> List.allPairs input
    |> List.filter (fun (a, b) -> a + b = 2020)
    |> List.tryHead
    |> Option.map (fun (a, b) -> a * b)
    
let calculateExpenses2 input =
    input
    |> List.allPairs input
    |> List.allPairs input
    |> List.map (fun (a, (b, c)) -> (a, b, c))
    |> List.filter (fun (a, b, c) -> a + b + c = 2020)
    |> List.tryHead
    |> Option.map (fun (a, b, c) -> a * b * c)
    
[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file
        input |> calculateExpenses1 |> Option.iter (printfn "%d")
        input |> calculateExpenses2 |> Option.iter (printfn "%d")
        0
    | _ ->
        printfn "Usage: Aoc.Day1 file"
        1
