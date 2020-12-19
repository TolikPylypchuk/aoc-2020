open System.IO

open FSharpPlus
open FSharpPlus.Data

open FParsec

type RuleLabel = RuleLabel of int

type Rule =
    | SimpleRule of string
    | CompositeRule of RuleLabel list

type Input = {
    Rules : Map<RuleLabel, Rule list>
    Strings : string list
}

let parseLabel : Parser<RuleLabel, unit> = attempt (spaces >>. pint32 .>> spaces |>> RuleLabel)

let parseSimpleRule = attempt (spaces >>. pchar '"' >>. many1CharsTill letter (pchar '"') .>> spaces |>> SimpleRule)

let parseOneCompositeRule = attempt (many parseLabel |>> CompositeRule)

let parseCompositeRules = sepBy parseOneCompositeRule (pchar '|')

let parseRule = parseSimpleRule |>> List.singleton <|> parseCompositeRules

let parseFullRule = parseLabel .>> pchar ':' .>>. parseRule

let isValid allRules label (stringToCheck : string) =
    let rec matchCompositeRule (index : int) labels =
        seq {
            match labels with
            | [] -> yield index
            | label :: otherLabels ->
                for index in matchRule index label do
                    yield! matchCompositeRule index otherLabels
        }
    and matchRule index label =
        allRules
        |> Map.tryFind label
        |> Option.defaultValue List.empty
        |> Seq.ofList
        |> Seq.bind (function
            | SimpleRule str -> seq { if stringToCheck.[index..] |> String.startsWith str then index + str.Length }
            | CompositeRule labels -> labels |> matchCompositeRule index)

    matchRule 0 label
    |> Seq.exists ((=) stringToCheck.Length)

let maybe parser str =
    match run parser str with
    | Success (result, _, _) -> Some result
    | _ -> None

let tryTail = function [] -> None | _ :: tail -> Some tail

let getInput file =
    let lines =
        file
        |> File.ReadAllLines
        |> List.ofArray

    monad {
        let! spaceIndex = lines |> List.tryFindIndex ((=) "")
        let rules, strings = lines |> List.splitAt spaceIndex
        let! strings = strings |> tryTail
        let! rules = rules |> List.map (maybe parseFullRule) |> sequence

        return { Rules = rules |> Map.ofList; Strings = strings }
    }

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        monad {
            let! input = file |> getInput
            do input.Strings
            |> List.filter (isValid input.Rules (RuleLabel 0))
            |> List.length
            |> printfn "%d"
        } |> ignore
        0
    | _ ->
        printfn "Usage: Aoc.Day19 file"
        1
