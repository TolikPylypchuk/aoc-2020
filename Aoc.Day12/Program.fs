open System
open System.IO
open System.Text.RegularExpressions

type MoveDirection =
    | North
    | South
    | East
    | West

type TurnDirection = Left | Right

type TurnAngle =
    | D0
    | D90
    | D180
    | D270

type Point = {
    X : int
    Y : int
}

type ShipState = {
    Position : Point
    CurrentDirection : MoveDirection
}

type ExtendedShipState = {
    ShipPosition : Point
    Waypoint : Point
}

type Instruction =
    | Move of MoveDirection * int
    | Turn of TurnDirection * TurnAngle
    | MoveForward of int
    
let (|Int|_|) (str : string) =
    match str |> Int32.TryParse with
    | true, result -> Some result
    | _ -> None
    
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let (|MoveDirection|_|) =
    function
    | "N" -> Some North
    | "S" -> Some South
    | "E" -> Some East
    | "W" -> Some West
    | _ -> None

let (|TurnDirection|_|) =
    function
    | "L" -> Some Left
    | "R" -> Some Right
    | _ -> None

let (|TurnAngle|_|) =
    function
    | "0" -> Some D0
    | "90" -> Some D90
    | "180" -> Some D180
    | "270" -> Some D270
    | _ -> None

let parseInstruction =
    let instruction = "^(.)(\\d+)$"
    function
    | Regex instruction [ MoveDirection direction; Int distance ] -> Some <| Move (direction, distance)
    | Regex instruction [ TurnDirection direction; TurnAngle angle ] -> Some <| Turn (direction, angle)
    | Regex instruction [ "F"; Int distance ] -> Some <| MoveForward distance
    | _ -> None

let getInput = File.ReadLines >> Seq.choose parseInstruction

let move point distance =
    function
    | North -> { point with Y = point.Y + distance }
    | South -> { point with Y = point.Y - distance }
    | East -> { point with X = point.X + distance }
    | West -> { point with X = point.X - distance }

let turn direction angle =
    match direction, angle with
    | Right, D90 -> function North -> East | East -> South | South -> West | West -> North
    | Left, D90 -> function North -> West | West -> South | South -> East | East -> North
    | _, D180 -> function North -> South | East -> West | South -> North | West -> East
    | Right, D270 -> function North -> West | West -> South | South -> East | East -> North
    | Left, D270 -> function North -> East | East -> South | South -> West | West -> North
    | _, D0 -> id

let turnAround point direction angle pointToTurn =
    let xDelta = pointToTurn.X - point.X
    let yDelta = pointToTurn.Y - point.Y

    match direction, angle with
    | Right, D90 -> { X = point.X + yDelta; Y = point.Y - xDelta }
    | Left, D90 -> { X = point.X - yDelta; Y = point.Y + xDelta }
    | _, D180 -> { X = point.X - xDelta; Y = point.Y - yDelta }
    | Right, D270 -> { X = point.X - yDelta; Y = point.Y + xDelta }
    | Left, D270 -> { X = point.X + yDelta; Y = point.Y - xDelta }
    | _, D0 -> point

let moveOnce state =
    function
    | Move (direction, distance) ->
        { state with Position = direction |> move state.Position distance }
    | MoveForward distance ->
        { state with Position = state.CurrentDirection |> move state.Position distance }
    | Turn (direction, angle) ->
        { state with CurrentDirection = turn direction angle state.CurrentDirection }

let moveOnceEx state =
    function
    | Move (direction, distance) ->
        { state with Waypoint = direction |> move state.Waypoint distance }
    | MoveForward distance ->
        let pos = state.ShipPosition
        let xDelta = state.Waypoint.X - pos.X
        let yDelta = state.Waypoint.Y - pos.Y
        let newPos = { X = pos.X + distance * xDelta; Y = pos.Y + distance * yDelta }
        { ShipPosition = newPos; Waypoint = { X = newPos.X + xDelta; Y = newPos.Y + yDelta } }
    | Turn (direction, angle) ->
        { state with Waypoint = state.Waypoint |> turnAround state.ShipPosition direction angle }

let getDistanceFromOrigin point = abs point.X + abs point.Y

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        file
        |> getInput
        |> Seq.fold moveOnce { Position = { X = 0; Y = 0 }; CurrentDirection = East }
        |> fun state -> state.Position
        |> getDistanceFromOrigin
        |> printfn "%d"

        file
        |> getInput
        |> Seq.fold moveOnceEx { ShipPosition = { X = 0; Y = 0 }; Waypoint = { X = 10; Y = 1 } }
        |> fun state -> state.ShipPosition
        |> getDistanceFromOrigin
        |> printfn "%d"

        0
    | _ ->
        printfn "Usage: Aoc.Day12 file"
        1
