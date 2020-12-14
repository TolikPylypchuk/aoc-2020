open System
open System.IO
open System.Text.RegularExpressions

type Mask = {
    And : int64
    Or : int64
}

type ProgramState = {
    Mask : string
    Memory : Map<int64, int64>
}

type Instruction =
    | UpdateMask of string
    | WriteToMemory of int64 * int64

type AddressTransformation =
    | DoNothing
    | Set1
    | Multiplex

let (|Int64|_|) (radix : int) (str : string) =
    try Convert.ToInt64(str, radix) |> Some with _ -> None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let parseMask (str : string) =
    match str.Replace('X', '1'), str.Replace('X', '0') with
    | Int64 2 andMask, Int64 2 orMask -> Some { And = andMask; Or = orMask }
    | _ -> None

let parseInstruction =
    function
    | Regex "mask = ([10X]{36})" [ mask ] -> mask |> UpdateMask |> Some
    | Regex @"mem\[(\d+)\] = (\d+)" [ Int64 10 address; Int64 10 value ] -> (address, value) |> WriteToMemory |> Some 
    | _ -> None

let transformAddresses addresses (bit, transformation) = [
    for address in addresses do
        match transformation with
        | DoNothing ->
            yield address
        | Set1 ->
            yield address ||| (1L <<< bit)
        | Multiplex ->
            yield address ||| (1L <<< bit)
            yield address &&& ~~~(1L <<< bit)
]

let maskAddress mask address =
    mask
    |> Seq.rev
    |> Seq.mapi (fun i -> function
        | '0' -> Some (i, DoNothing)
        | '1' -> Some (i, Set1)
        | 'X' -> Some (i, Multiplex)
        | _ -> None)
    |> Seq.choose id
    |> Seq.fold transformAddresses [ address ]

let updateMemory value state address =
    if value = 0L
    then { state with Memory = state.Memory |> Map.remove address }
    else { state with Memory = state.Memory |> Map.add address value }

let execute1 state =
    function
    | UpdateMask mask ->
        { state with Mask = mask }
    | WriteToMemory (address, value) ->
        match state.Mask |> parseMask with
        | Some mask -> 
            let actualValue = (value &&& mask.And) ||| mask.Or
            updateMemory actualValue state address
        | _ ->
            state

let execute2 state =
    function
    | UpdateMask mask ->
        { state with Mask = mask }
    | WriteToMemory (address, value) ->
        address
        |> maskAddress state.Mask
        |> List.fold (updateMemory value) state

let initialState = { Mask = "0" |> String.replicate 36; Memory = Map.empty }

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> List.choose parseInstruction

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let output state =
            state.Memory
            |> Map.toList
            |> List.map snd
            |> List.fold (+) 0L
            |> printfn "%d"

        let input = getInput file

        input
        |> List.fold execute1 initialState
        |> output

        input
        |> List.fold execute2 initialState
        |> output

        0
    | _ ->
        printfn "Usage: Aoc.Day14 file"
        1
