open System
open System.IO

type Schedule = {
    ArrivalTimestamp : int
    DepartureTimestamps : (int * int) list
}

let (|Int|_|) (str : string) =
    match str |> Int32.TryParse with
    | true, result -> Some result
    | _ -> None

let parseDepartures (str : string) =
    str.Split(',')
    |> List.ofArray
    |> List.mapi (fun index -> function Int num -> Some (index, num) | _ -> None)
    |> List.choose id

let getInput file =
    match file |> File.ReadAllLines with
    | [| Int arrival; departures |] ->
        Some { ArrivalTimestamp = arrival; DepartureTimestamps = departures |> parseDepartures }
    | _ ->
        None

let getWaitTime schedule departure = departure - schedule.ArrivalTimestamp % departure

let getBestDeparture schedule =
    schedule.DepartureTimestamps
    |> List.map snd
    |> List.minBy (getWaitTime schedule)
    |> fun bestDeparture -> bestDeparture, getWaitTime schedule bestDeparture

let getInverseNumbers r1 r2 =
    let rec getInverse r1 r2 x1 x2 y1 y2 =
        let r3 : int64 = r1 - r2 * (r1 / r2)
        let x3 = x1 - x2 * (r1 / r2)
        let y3 = y1 - y2 * (r1 / r2)

        if r3 <> 0L
        then getInverse r2 r3 x2 x3 y2 y3
        else x2, y2

    getInverse (max r1 r2) (min r1 r2) 1L 0L 0L 1L

let rec makePositive adder result =
    if result >= 0L
    then result
    else (result + adder) |> makePositive adder

let findSpecialTimestamp schedule =
    let departures = schedule.DepartureTimestamps |> List.map (fun (index, departure) -> index, int64 departure)
    let product = departures |> List.map snd |> List.fold (*) 1L

    departures
    |> List.zip (departures |> List.map snd |> List.map ((/) product))
    |> List.map (fun (productByDeparture, (index, departure)) ->
        int64 (-index) % departure, productByDeparture, getInverseNumbers departure productByDeparture |> fst)
    |> List.map (fun (x, y, z) -> x * y * z)
    |> List.fold (+) 0L
    |> makePositive product

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let schedule = getInput file

        schedule
        |> Option.map getBestDeparture
        |> Option.map (fun (departure, waitTime) -> departure * waitTime)
        |> Option.iter (printfn "%d")

        schedule
        |> Option.map findSpecialTimestamp
        |> Option.iter (printfn "%d")

        0
    | _ ->
        printfn "Usage: Aoc.Day13 file"
        1
