open System.IO

let getAdjacentCells3D (plane, row, col) (cells : int * int * int -> bool) =
    let getCellsInRow plane' row' = seq {
        yield cells (plane', row', col - 1)

        if row' <> row || plane' <> plane then 
            yield cells (plane', row', col)

        yield cells (plane', row', col + 1)
    }

    let getCellsInPlane plane' = seq {
        yield! getCellsInRow plane' <| row - 1
        yield! getCellsInRow plane' row
        yield! getCellsInRow plane' <| row + 1
    }

    seq {
        yield! getCellsInPlane <| plane - 1
        yield! getCellsInPlane plane
        yield! getCellsInPlane <| plane + 1
    }

let getAdjacentCells4D (space, plane, row, col) (cells : int * int * int * int -> bool) =
    let getCellsInRow space' plane' row' = seq {
        yield cells (space', plane', row', col - 1)

        if row' <> row || plane' <> plane || space' <> space then 
            yield cells (space', plane', row', col)

        yield cells (space', plane', row', col + 1)
    }

    let getCellsInPlane space' plane' = seq {
        yield! getCellsInRow space' plane' <| row - 1
        yield! getCellsInRow space' plane' row
        yield! getCellsInRow space' plane' <| row + 1
    }
    
    let getCellsInSpace space' = seq {
        yield! getCellsInPlane space' <| plane - 1
        yield! getCellsInPlane space' plane
        yield! getCellsInPlane space' <| plane + 1
    }

    seq {
        yield! getCellsInSpace <| space - 1
        yield! getCellsInSpace space
        yield! getCellsInSpace <| space + 1
    }

let getOldCell3D oldCells (plane, row, col) =
    let numPlanes = oldCells |> Array3D.length1
    let numRows = oldCells |> Array3D.length2
    let numCols = oldCells |> Array3D.length3

    if plane < 1 || row < 1 || col < 1 || plane > numPlanes || row > numRows || col > numCols then
        false
    else
        oldCells.[plane - 1, row - 1, col - 1]

let getOldCell4D oldCells (space, plane, row, col) =
    let numSpaces = oldCells |> Array4D.length1
    let numPlanes = oldCells |> Array4D.length2
    let numRows = oldCells |> Array4D.length3
    let numCols = oldCells |> Array4D.length4

    if space < 1 || plane < 1 || row < 1 || col < 1 || space > numSpaces || plane > numPlanes || row > numRows || col > numCols then
        false
    else
        oldCells.[space - 1, plane - 1, row - 1, col - 1]

let calculateNewCell3D oldCells plane row col =
    let numPlanes = oldCells |> Array3D.length1
    let numRows = oldCells |> Array3D.length2
    let numCols = oldCells |> Array3D.length3

    let numAdjacentActiveCells =
        getAdjacentCells3D (plane, row, col) (getOldCell3D oldCells)
        |> Seq.filter id
        |> Seq.length

    let oldCell =
        if plane < 1 || row < 1 || col < 1 || plane > numPlanes || row > numRows || col > numCols
        then false
        else oldCells.[plane - 1, row - 1, col - 1]

    if oldCell
    then numAdjacentActiveCells = 2 || numAdjacentActiveCells = 3
    else numAdjacentActiveCells = 3

let calculateNewCell4D oldCells space plane row col =
    let numSpaces = oldCells |> Array4D.length1
    let numPlanes = oldCells |> Array4D.length2
    let numRows = oldCells |> Array4D.length3
    let numCols = oldCells |> Array4D.length4

    let numAdjacentActiveCells =
        getAdjacentCells4D (space, plane, row, col) (getOldCell4D oldCells)
        |> Seq.filter id
        |> Seq.length

    let oldCell =
        if space < 1 || plane < 1 || row < 1 || col < 1 || space > numSpaces || plane > numPlanes || row > numRows || col > numCols
        then false
        else oldCells.[space - 1, plane - 1, row - 1, col - 1]

    if oldCell
    then numAdjacentActiveCells = 2 || numAdjacentActiveCells = 3
    else numAdjacentActiveCells = 3

let calculateNewState3D cells =
    let numPlanes = (cells |> Array3D.length1) + 2
    let numRows = (cells |> Array3D.length2) + 2
    let numCols = (cells |> Array3D.length3) + 2

    calculateNewCell3D cells
    |> Array3D.init numPlanes numRows numCols

let calculateNewState4D cells =
    let numSpaces = (cells |> Array4D.length1) + 2
    let numPlanes = (cells |> Array4D.length2) + 2
    let numRows = (cells |> Array4D.length3) + 2
    let numCols = (cells |> Array4D.length4) + 2

    calculateNewCell4D cells
    |> Array4D.init numSpaces numPlanes numRows numCols

let flatArray3D array =
    Array3D.init 1 (array |> Array2D.length1) (array |> Array2D.length2) (fun _ row col -> array.[row, col])

let flatArray4D array =
    Array4D.init
        1
        (array |> Array3D.length1)
        (array |> Array3D.length2)
        (array |> Array3D.length3)
        (fun _ plane row col -> array.[plane, row, col])

let getInput =
    File.ReadAllLines
    >> Array.map (Seq.map <| function '#' -> true | _ -> false)
    >> array2D

let numActiveCellsAfter newState numCycles cells =
    List.replicate numCycles ()
    |> List.fold (fun state _ -> newState state) cells
    |> Seq.cast<bool>
    |> Seq.filter id
    |> Seq.length

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let cells = file |> getInput |> flatArray3D

        cells
        |> numActiveCellsAfter calculateNewState3D 6
        |> printfn "%d"
        
        cells
        |> flatArray4D
        |> numActiveCellsAfter calculateNewState4D 6
        |> printfn "%d"
        0
    | _ ->
        printfn "Usage: Aoc.Day17 file"
        1
