open System.IO

open FParsec

let parseWord = many1CharsTill letter (pchar ' ')

let pBagsContain = pstring "bags contain "
let pBagOrBags = (pstring "bags") <|> (pstring "bag")
let pComma = pstring ", "
let pPeriod = pchar '.'

let parseBagColor =
    spaces >>. parseWord .>>. parseWord |>> (fun (shade, color) -> $"{shade} {color}")

let parseBagRule : Parser<string * (int * string) list, unit> =
    parseBagColor .>> pBagsContain .>>. (sepBy1 (pint32 .>>. parseBagColor .>> pBagOrBags) pComma) .>> (pPeriod .>> eof)

let rec canContainColor bagRules colorToFind rules =
    rules
    |> List.map snd
    |> List.map (fun ruleColor ->
        ruleColor = colorToFind ||
        canContainColor bagRules colorToFind (bagRules |> Map.tryFind ruleColor |> Option.defaultValue []))
    |> List.fold (||) false

let possibleTopLevelColors colorToFind bagRules =
    bagRules
    |> Map.map (fun _ -> canContainColor bagRules colorToFind)
    |> Map.toList
    |> List.filter snd
    |> List.map fst

let rec countNeededBags color bagRules =
    bagRules
    |> Map.tryFind color
    |> Option.defaultValue []
    |> List.map (fun (count, color) -> count * (countNeededBags color bagRules + 1))
    |> List.fold (+) 0

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> List.map (run parseBagRule)
    >> List.choose (function Success (result, _, _) -> Some result | _ -> None)
    >> Map.ofList

[<EntryPoint>]
let main argv =
    match argv with
    | [| file; color |] ->
        let bagRules = file |> getInput

        bagRules
        |> possibleTopLevelColors color
        |> List.length
        |> printfn "%d"

        bagRules
        |> countNeededBags color
        |> printfn "%d"
        0
    | _ ->
        printfn "Usage: Aoc.Day7 file"
        1
