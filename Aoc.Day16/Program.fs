open System.IO

open FSharpPlus
open FSharpPlus.Data

open FParsec

type Ticket = Ticket of int list

type Rule = {
    Name : string
    Validate : int -> bool
}

type Input = {
    Rules : Rule list
    MyTicket : Ticket
    OtherTickets : Ticket list
}

type InputScanner =
    | Complete of string list
    | Incomplete of string list

let validate ranges num =
    ranges |> List.exists (fun (low, high) -> low <= num && num <= high)

let parseRange : Parser<int * int, unit> =
    spaces >>. pint32 .>> pchar '-' .>>. pint32 .>> spaces

let parseRule =
    many1CharsTill anyChar (pchar ':') .>>. sepBy1 parseRange (pstring "or")
    |>> (fun (name, ranges) -> { Name = name; Validate = validate ranges })

let parseTicket : Parser<Ticket, unit> =
    sepBy pint32 (pchar ',') |>> Ticket

let maybe parser = run parser >> function Success (result, _, _) -> Some result | _ -> None

let parseInput rules myTicket otherTickets = monad {
    let! rules = rules |> List.map (maybe parseRule) |> sequence
    let! myTicket = myTicket |> maybe parseTicket
    let! otherTickets = otherTickets |> List.map (maybe parseTicket) |> sequence
    return { Rules = rules; MyTicket = myTicket; OtherTickets = otherTickets }
}

let scanLine scanner line =
    match scanner with
    | Complete _ -> if line = "" then Incomplete [] else Incomplete [ line ]
    | Incomplete lines -> if line = "" then Complete lines else Incomplete <| lines @ [ line ]

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> fun lines -> lines @ [ "" ]
    >> List.scan scanLine (Incomplete [])
    >> List.choose (function Complete lines -> Some lines | _ -> None)
    >> function [ rules; [ _; myTicket ]; _ :: otherTickets ] -> parseInput rules myTicket otherTickets | _ -> None

let getInvalidFields rules (Ticket ticket) =
    let isValid =
        rules
        |> List.map (fun rule -> rule.Validate)
        |> List.fold (fun a b -> fun num -> a num || b num) (fun _ -> false)
    
    ticket |> List.filter (not << isValid)

let discardInvalidTickets input =
    { input with OtherTickets = input.OtherTickets |> List.filter (getInvalidFields input.Rules >> List.isEmpty) }

let getFieldIndexes tickets rule =
    tickets
    |> List.map (fun (Ticket ticket) -> ticket)
    |> List.transpose
    |> List.indexed
    |> List.filter (fun (_, nums) -> nums |> List.exists (not << rule.Validate) |> not)
    |> List.map fst
    |> fun indexes -> rule.Name, (indexes |> Set.ofList)

let matchFieldsToRules input =
    input.Rules
    |> List.map (getFieldIndexes input.OtherTickets)
    |> List.sortBy (snd >> Set.count)
    |> List.fold
        (fun (list, encounteredIndexes) (rule, indexes) ->
            (rule, Set.difference indexes encounteredIndexes) :: list, Set.union indexes encounteredIndexes)
        ([], Set.empty)
    |> fst
    |> List.map (fun (rule, indexes) ->
        indexes |> Set.toList |> List.tryExactlyOne |> Option.map (fun index -> rule, index))
    |> sequence

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        monad {
            let! input = file |> getInput
            do input.OtherTickets
                |> List.map (getInvalidFields input.Rules)
                |> List.sumBy List.sum
                |> printfn "%d"

            let! matchedFields = input |> discardInvalidTickets |> matchFieldsToRules

            matchedFields
            |> List.filter (fst >> String.startsWith "departure")
            |> List.map snd
            |> List.map (fun index -> let (Ticket ticket) = input.MyTicket in ticket |> List.item index)
            |> List.map int64
            |> List.fold (*) 1L
            |> printfn "%d"
        } |> ignore

        0
    | _ ->
        printfn "Usage: Aoc.Day16 file"
        1
