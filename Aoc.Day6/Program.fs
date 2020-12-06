open System.IO

type AnsweredQuestions = AnsweredQuestions of Set<char> list

type AccumulatedLine = Incomplete of string list | Complete of string

let getQuestions (AnsweredQuestions questions) = questions

let flip f a b = f b a

let parseAnsweredQuestions (str : string) =
    str.Split(' ')
    |> List.ofArray
    |> List.map Set.ofSeq
    |> AnsweredQuestions

let accumulateNonEmptyLines scannedLine line =
    match (scannedLine, line) with
    | (Incomplete [], "") -> Incomplete []
    | (Incomplete lines, "") -> lines |> List.reduce (flip <| sprintf "%s %s") |> Complete
    | (Incomplete lines, line) -> line :: lines |> Incomplete
    | (Complete _, "") -> Incomplete []
    | (Complete _, line) -> Incomplete [ line ]

let readAndReduceLines file =
    Seq.append (File.ReadLines(file)) (Seq.singleton "")
    |> Seq.scan accumulateNonEmptyLines (Incomplete [])
    |> Seq.choose (function Complete str -> Some str | _ -> None)

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        file
        |> readAndReduceLines
        |> Seq.map parseAnsweredQuestions
        |> Seq.map getQuestions
        |> Seq.filter (not << List.isEmpty)
        |> Seq.map (fun sets -> sets |> List.reduce Set.union, sets |> List.reduce Set.intersect)
        |> Seq.map (fun (any, all) -> any |> Set.count, all |> Set.count)
        |> Seq.fold (fun (accAny, accAll) (any, all) -> accAny + any, accAll + all) (0, 0)
        |> fun (any, all) -> printfn "%d\n%d" any all
        0
    | _ ->
        printfn "Usage: Aoc.Day6 file"
        1
