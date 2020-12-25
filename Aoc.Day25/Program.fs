open System
open System.IO

type Key = Key of int64

type KeyPair = {
    CardKey : Key
    DoorKey : Key
}

let (|Int64|_|) (str : string) =
    match str |> Int64.TryParse with
    | true, result -> Some result
    | _ -> None

let getInput =
    File.ReadAllLines
    >> function [| Int64 roomKey; Int64 doorKey |] -> Some { CardKey = Key roomKey; DoorKey = Key doorKey } | _ -> None

let powerModulo base' exponent modulus =
    let rec powerModulo' base' exponent result =
        if exponent = 0L
        then result
        else
            let nextBase = (base' * base') % modulus
            let nextExponent = exponent / 2L
            let nextResult = if exponent % 2L = 1L then (result * base') % modulus else result
            powerModulo' nextBase nextExponent nextResult

    powerModulo' (base' % modulus) exponent 1L

let multiplyModulo base' factor modulus =
    let rec powerModulo' base' factor result =
        if factor = 0L
        then result
        else
            let nextBase = (base' * 2L) % modulus
            let nextFactor = factor / 2L
            let nextResult = if factor % 2L = 1L then (result + base') % modulus else result
            powerModulo' nextBase nextFactor nextResult

    powerModulo' (base' % modulus) factor 0L

let findLoopSize firstNumber modulus (Key key) =
    let rec findLoopSize' number result =
        if number = key
        then result
        else findLoopSize' (multiplyModulo number firstNumber modulus) (result + 1)

    findLoopSize' firstNumber 1

let findEncryptionKey subjectNumber modulo { CardKey = cardKey; DoorKey = (Key doorKey) } =
    let loopSize = cardKey |> findLoopSize subjectNumber modulo
    powerModulo doorKey (int64 loopSize) modulo

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        file
        |> getInput
        |> Option.map (findEncryptionKey 7L 20201227L)
        |> Option.iter (printfn "%d")
        0
    | _ ->
        printfn "Usage: Aoc.Day25 file"
        1
