open System
open System.IO
open System.Text.RegularExpressions

open FSharpPlus
open FSharpPlus.Data

type Operation =
    | Accumulate
    | Jump
    | NoOperation

type Instruction = {
    Line : int
    Operation : Operation
    Argument : int
}

type InstructionRepairWrapper = {
    Instructions : Instruction[]
    RepairedLine : int
}

type Instructions =
    | Bare of Instruction[]
    | Wrapped of InstructionRepairWrapper

type ProgramState = {
    Instructions : Instructions
    CurrentLine : int
    VisitedLines : Set<int>
    Terminated : bool
}

let (|Int|_|) (str : string) =
    match str |> Int32.TryParse with
    | true, result -> Some result
    | _ -> None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let mapOptionalTuple =
    function
    | Some a, Some b -> Some (a, b)
    | _ -> None

let findIndex from pred array = Array.FindIndex(array, from, Predicate<'a>(pred))

let splitByOneSpace (str : string) =
    match str.Split() with
    | [| a; b |] -> Some (a, b)
    | _ -> None

let shouldRepair instruction =
    match instruction.Operation with
    | Jump | NoOperation -> true
    | _ -> false

let repairOperation instruction =
    match instruction.Operation with
    | Jump -> { instruction with Operation = NoOperation }
    | NoOperation -> { instruction with Operation = Jump }
    | _ -> instruction

let repair startLine instructions =
    let lineToRepair = instructions |> findIndex startLine shouldRepair
    { Instructions = instructions; RepairedLine = lineToRepair }

let getRepairedInstruction index wrapper =
    if index = wrapper.RepairedLine
    then repairOperation wrapper.Instructions.[index]
    else wrapper.Instructions.[index]

let at index =
    function
    | Bare instructions -> instructions.[index]
    | Wrapped wrapper -> wrapper |> getRepairedInstruction index

let length =
    function
    | Bare instructions -> instructions.Length
    | Wrapped wrapper -> wrapper.Instructions.Length

let unwrap =
    function
    | Bare instructions -> instructions
    | Wrapped wrapper -> wrapper.Instructions

let parseOperation =
    function
    | "acc" -> Some Accumulate
    | "jmp" -> Some Jump
    | "nop" -> Some NoOperation
    | _ -> None

let parseArgument =
    function
    | Regex @"\+(\d+)" [ Int num ] -> Some num
    | Regex @"-(\d+)" [ Int num ] -> Some (-num)
    | _ -> None

let parseInstruction (line, str) =
    str
    |> splitByOneSpace
    |> Option.map (fun (op, arg) -> parseOperation op, parseArgument arg)
    |> Option.bind mapOptionalTuple
    |> Option.map (fun (op, arg) -> { Line = line; Operation = op; Argument = arg })

let getInput file =
    file
    |> File.ReadLines
    |> Seq.indexed
    |> Seq.map parseInstruction
    |> sequence
    |> Option.map Array.ofSeq
    |> Option.defaultValue [||]

let executeInstruction acc = monad {
    let! state = State.get

    if state.CurrentLine = (state.Instructions |> length) then
        do! State.put { state with Terminated = true }
        return acc
    else
        let instruction = state.Instructions |> at state.CurrentLine
        let visitedLines = state.VisitedLines |> Set.add state.CurrentLine
        let currentLine = state.CurrentLine
        do! State.put { state with CurrentLine = currentLine + 1; VisitedLines = visitedLines }

        match instruction.Operation with
        | Accumulate ->
            return acc + instruction.Argument
        | Jump ->
            do! State.put { state with CurrentLine = currentLine + instruction.Argument }
            return acc
        | NoOperation ->
            return acc
}

let rec execute acc = monad {
    let! newAcc = executeInstruction acc
    let! newState = State.get

    if newState.VisitedLines |> Set.contains newState.CurrentLine || newState.Terminated
    then return newAcc
    else return! execute newAcc
}

let rec executeRepaired acc startLine = monad {
    let! state = State.get
    let wrapper = state.Instructions |> unwrap |> repair startLine
    do! State.put { state with Instructions = Wrapped wrapper }

    let! result = execute acc
    let! newState = State.get

    if newState.Terminated then
        return result
    else
        do! put state
        return! executeRepaired acc (wrapper.RepairedLine + 1)
}

let initialState instructions = {
    Instructions = Bare instructions
    CurrentLine = 0
    VisitedLines = Set.empty
    Terminated = false
}

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let state =
            file
            |> getInput
            |> initialState

        state
        |> State.eval (execute 0)
        |> printfn "%d"
        
        state
        |> State.eval (executeRepaired 0 0)
        |> printfn "%d"
        0
    | _ ->
        printfn "Usage: Aoc.Day8 file"
        1
