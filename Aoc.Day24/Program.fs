open System.IO
open System.Text.RegularExpressions

type XOffset = NoOffset | RightOffset

type Tile = {
    X : int
    XOffset : XOffset
    Y : int
}

type TileReference =
    | East
    | NorthEast
    | SouthEast
    | West
    | NorthWest
    | SouthWest

type FlippedTiles = FlippedTiles of Set<Tile>

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let containedIn set a = set |> Set.contains a

let getAdjacentTile tile =
    function
    | East ->
        { tile with X = tile.X + 1 }
    | NorthEast ->
        match tile.XOffset with
        | NoOffset -> { tile with Y = tile.Y + 1; XOffset = RightOffset }
        | RightOffset -> { tile with X = tile.X + 1; Y = tile.Y + 1; XOffset = NoOffset }
    | SouthEast ->
        match tile.XOffset with
        | NoOffset -> { tile with Y = tile.Y - 1; XOffset = RightOffset }
        | RightOffset -> { tile with X = tile.X + 1; Y = tile.Y - 1; XOffset = NoOffset }
    | West ->
        { tile with X = tile.X - 1 }
    | NorthWest ->
        match tile.XOffset with
        | NoOffset -> { tile with X = tile.X - 1; Y = tile.Y + 1; XOffset = RightOffset }
        | RightOffset -> { tile with Y = tile.Y + 1; XOffset = NoOffset }
    | SouthWest ->
        match tile.XOffset with
        | NoOffset -> { tile with X = tile.X - 1; Y = tile.Y - 1; XOffset = RightOffset }
        | RightOffset -> { tile with Y = tile.Y - 1; XOffset = NoOffset }

let getAllAdjacentTiles tile = [
    getAdjacentTile tile East
    getAdjacentTile tile NorthEast
    getAdjacentTile tile SouthEast
    getAdjacentTile tile West
    getAdjacentTile tile NorthWest
    getAdjacentTile tile SouthWest
]

let rec parseTileReferences tiles =
    match tiles with
    | Regex "^(e)(.*)$" [ _; nextTiles ] -> East :: (nextTiles |> parseTileReferences)
    | Regex "^(ne)(.*)$" [ _; nextTiles ] -> NorthEast :: (nextTiles |> parseTileReferences)
    | Regex "^(se)(.*)$" [ _; nextTiles ] -> SouthEast :: (nextTiles |> parseTileReferences)
    | Regex "^(w)(.*)$" [ _; nextTiles ] -> West :: (nextTiles |> parseTileReferences)
    | Regex "^(nw)(.*)$" [ _; nextTiles ] -> NorthWest :: (nextTiles |> parseTileReferences)
    | Regex "^(sw)(.*)$" [ _; nextTiles ] -> SouthWest :: (nextTiles |> parseTileReferences)
    | _ -> []

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> List.map parseTileReferences

let originTile = { X = 0; Y = 0; XOffset = NoOffset }

let findTile = List.fold getAdjacentTile

let flipTile (FlippedTiles flippedTiles) tile =
    if flippedTiles |> Set.contains tile
    then flippedTiles |> Set.remove tile |> FlippedTiles
    else flippedTiles |> Set.add tile |> FlippedTiles

let flipTilesInitial tileReferences =
    tileReferences
    |> List.map (findTile originTile)
    |> List.fold flipTile (FlippedTiles Set.empty)

let shouldFlipFlippedTile flippedTiles tile =
    tile
    |> getAllAdjacentTiles
    |> List.filter (containedIn flippedTiles)
    |> List.length
    |> fun count -> count = 0 || count > 2

let shouldFlipNonFlippedTile flippedTiles tile =
    tile
    |> getAllAdjacentTiles
    |> List.filter (containedIn flippedTiles)
    |> List.length
    |> (=) 2

let flipTiles (FlippedTiles flippedTiles) =
    flippedTiles
    |> Set.toList
    |> List.map (fun tile -> tile :: (tile |> getAllAdjacentTiles))
    |> List.concat
    |> List.distinct
    |> List.partition (containedIn flippedTiles)
    |> fun (flipped, nonFlipped) ->
        (flipped |> List.filter (not << shouldFlipFlippedTile flippedTiles)) @
        (nonFlipped |> List.filter (shouldFlipNonFlippedTile flippedTiles))
    |> Set.ofList
    |> FlippedTiles

let unfold func state =
    let newState = state |> func
    Some (newState, newState)

let getTiles (FlippedTiles tiles) = tiles

let printCount (FlippedTiles tiles) =
    tiles
    |> Set.count
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let initialTiles =
            file
            |> getInput
            |> flipTilesInitial

        initialTiles |> printCount

        initialTiles
        |> Seq.unfold (unfold flipTiles)
        |> Seq.skip 99
        |> Seq.head
        |> printCount

        0
    | _ ->
        printfn "Usage: Aoc.Day23 file"
        1
