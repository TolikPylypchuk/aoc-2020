open System
open System.IO
open System.Text.RegularExpressions

open FSharpPlus

type Cell = Empty | Full | Monster

type Tile = {
    Id : int
    Cells : Cell[,]
}

type Border = Top | Bottom | Left | Right

type InputScanner =
    | Complete of string list
    | Incomplete of string list

let (|Int|_|) (str : string) =
    match str |> Int32.TryParse with
    | true, result -> Some result
    | _ -> None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let getBorder border (tile : 'a[,]) =
    match border with
    | Top -> tile.[0, *]
    | Bottom -> tile.[(tile |> Array2D.length1) - 1, *]
    | Left -> tile.[*, 0]
    | Right -> tile.[*, (tile |> Array2D.length2) - 1]
    |> List.ofArray

let transform rotate getCell cells =
    let height = cells |> Array2D.length1
    let width = cells |> Array2D.length2

    let init = if not rotate then Array2D.init height width else Array2D.init width height

    getCell cells height width |> init

let flipX = transform false (fun cells _ width row col -> cells.[row, width - col - 1])

let flipY = transform false (fun cells height _ row col -> cells.[height - row - 1, col])

let rotateRight = transform true (fun cells height _ row col -> cells.[height - col - 1, row])

let rotateLeft = transform true (fun cells _ width row col -> cells.[col, width - row - 1])

let withTransformations cells =
    let transformations = [ id; flipX; flipY; rotateLeft; rotateRight ]

    transformations
    |> List.allPairs transformations
    |> List.map (fun (a, b) -> a >> b)
    |> List.map (fun transform -> transform cells)

let getAdjacency tile1 tile2 =
    let cells1 = tile1.Cells
    let cells2 = tile2.Cells

    if (cells1 |> getBorder Top) = (cells2 |> getBorder Bottom) then
        Some Top
    elif (cells1 |> getBorder Bottom) = (cells2 |> getBorder Top) then
        Some Bottom
    elif (cells1 |> getBorder Left) = (cells2 |> getBorder Right) then
        Some Left
    elif (cells1 |> getBorder Right) = (cells2 |> getBorder Left) then
        Some Right
    else
        None

let withAddedTile row col border tileToAdd picture =
    let height = picture |> Array2D.length1
    let width = picture |> Array2D.length2

    let row, col, picture =
        if row = 0 && border = Top then
            row + 1, col, Array2D.init height width (fun row col -> if row = 0 then None else picture.[row - 1, col])
        elif col = 0 && border = Left then
            row, col + 1, Array2D.init height width (fun row col -> if col = 0 then None else picture.[row, col - 1])
        else
            row, col, picture

    let newRow, newCol =
        match border with
        | Top -> row - 1, col
        | Bottom -> row + 1, col
        | Left -> row, col - 1
        | Right -> row, col + 1

    picture
    |> Array2D.mapi (fun picRow picCol tile -> if picRow = newRow && picCol = newCol then Some tileToAdd else tile)

let addTile tile picture =
    let transformedTiles = tile.Cells |> withTransformations |> List.map (fun cells -> { tile with Cells = cells })

    picture
    |> Array2D.mapi (fun row col tile -> (row, col, tile))
    |> Seq.cast<int * int * Tile option>
    |> Seq.choose (fun (row, col, tile) -> tile |> Option.map (fun tile -> (row, col, tile)))
    |> Seq.bind (fun (row, col, tile) ->
        transformedTiles
        |> List.choose (fun tileToAdd ->
            getAdjacency tile tileToAdd
            |> Option.map (fun border -> (row, col, border, tileToAdd)))
        |> List.toSeq)
    |> Seq.tryHead
    |> Option.map (fun (row, col, border, tileToAdd) -> picture |> withAddedTile row col border tileToAdd)

let initPicture height width firstTile =
    Array2D.init height width (fun row col -> if row = 0 && col = 0 then Some firstTile else None)

let rec populateTiles tiles picture =
    let result, remaining =
        tiles
        |> List.fold
            (fun (picture, remainingTiles) tile ->
                match picture |> addTile tile with
                | Some newPicture -> newPicture, remainingTiles
                | None -> picture, tile :: remainingTiles)
            (picture, [])

    if remaining |> List.isEmpty
    then result
    else result |> populateTiles remaining

let removeBorders tile =
    let height = tile |> Array2D.length1
    let width = tile |> Array2D.length2

    Array2D.init (height - 2) (width - 2) (fun row col -> tile.[row + 1, col + 1])

let flattenPicture (picture : Tile option[,]) =
    let reducedPicture =
        picture
        |> Array2D.map (Option.defaultValue { Id = 0; Cells = Array2D.zeroCreate 1 1 })
        |> Array2D.map (fun tile -> tile.Cells)
        |> Array2D.map removeBorders
        
    let pictureHeight = reducedPicture |> Array2D.length1
    let pictureWidth = reducedPicture |> Array2D.length2

    let tileHeight = reducedPicture.[0, 0] |> Array2D.length1
    let tileWidth = reducedPicture.[0, 0] |> Array2D.length2

    Array2D.init (pictureHeight * tileHeight) (pictureWidth * tileWidth) (fun row col ->
        let picRow, tRow = row / tileHeight, row % tileHeight
        let picCol, tCol = col / tileWidth, col % tileWidth
        reducedPicture.[picRow, picCol].[tRow, tCol])

let countFullCells (picture : Cell[,]) =
    picture
    |> Seq.cast<Cell>
    |> Seq.filter ((=) Full)
    |> Seq.length

let isFull = function Full -> true | _ -> false

let monsterBody (row, col) =
    [
        row + 1, col - 18
        row + 2, col - 17

        row + 2, col - 14
        row + 1, col - 13
        row + 1, col - 12
        row + 2, col - 11

        row + 2, col - 8
        row + 1, col - 7
        row + 1, col - 6
        row + 2, col - 5
        
        row + 2, col - 2
        row + 1, col - 1
        row + 1, col
        row, col
        row + 1, col + 1
    ]

let isMonsterHead (picture : Cell[,]) (row, col) =
    if picture.[row, col] |> isFull |> not then
        false
    else
        let height = picture |> Array2D.length1
        let width = picture |> Array2D.length2

        if row + 2 < height && col + 1 < width && col >= 18 then
            (row, col)
            |> monsterBody
            |> List.map (fun (row, col) -> isFull picture.[row, col])
            |> List.fold (&&) true
        else
            false

let findMonsterHeads picture =
    picture
    |> Array2D.mapi (fun row col _ -> row, col)
    |> Seq.cast<int * int>
    |> Seq.filter (isMonsterHead picture)
    |> List.ofSeq

let markMonsters picture =
    let monsters =
        picture
        |> findMonsterHeads
        |> List.map monsterBody
        |> List.concat
        
    let height = picture |> Array2D.length1
    let width = picture |> Array2D.length2

    Array2D.init height width (fun row col -> if monsters |> List.contains (row, col) then Monster else picture.[row, col])

let scanLine scanner line =
    match scanner with
    | Complete _ -> if line = "" then Incomplete [] else Incomplete [ line ]
    | Incomplete lines -> if line = "" then Complete lines else Incomplete <| lines @ [ line ]

let parseCell = function '#' -> Some Full | '.' -> Some Empty | _ -> None

let parseTile header lines =
    match header with
    | Regex @"^Tile (\d+):$" [ Int id ] ->
        lines
        |> List.map (Seq.map parseCell >> sequence >> (Option.map Array.ofSeq))
        |> sequence
        |> Option.map Array.ofList
        |> Option.map (fun (cells : Cell[][]) -> Array2D.init cells.Length cells.Length (fun row col -> cells.[row].[col]))
        |> Option.map (fun cells -> { Id = id; Cells = cells })
    | _ -> None

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> fun lines -> lines @ [ "" ]
    >> List.scan scanLine (Incomplete [])
    >> List.choose (function Complete lines -> Some lines | _ -> None)
    >> List.choose (function header :: lines -> parseTile header lines | _ -> None)

let getBorderTileIds (picture : Tile option[,]) =
    let height = picture |> Array2D.length1
    let width = picture |> Array2D.length2

    match picture.[0, 0], picture.[height - 1, 0], picture.[0, width - 1], picture.[height - 1, width - 1] with
    | Some tile1, Some tile2, Some tile3, Some tile4 -> [ tile1.Id; tile2.Id; tile3.Id; tile4.Id ]
    | _ -> []

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let allTiles = file |> getInput
        let length = allTiles |> List.length |> sqrt

        match allTiles with
        | firstTile :: otherTiles ->
            let picture =
                initPicture length length firstTile
                |> populateTiles otherTiles

            picture
            |> getBorderTileIds
            |> List.map int64
            |> List.fold (*) 1L
            |> printfn "%d"
            
            let picture = picture |> flattenPicture

            let numAllFullCells = picture |> countFullCells

            picture
            |> withTransformations
            |> List.map markMonsters
            |> List.map countFullCells
            |> List.filter ((<>) numAllFullCells)
            |> List.tryHead
            |> Option.iter (printfn "%d")
        | _ -> ()
        0
    | _ ->
        printfn "Usage: Aoc.Day20 file"
        1
