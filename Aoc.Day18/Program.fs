open System.IO

open FParsec

type Expression =
    | Number of int64
    | Addition of Expression * Expression
    | Multiplication of Expression * Expression

let rec execute =
    function
    | Number num -> num
    | Addition (left, right) -> execute left + execute right
    | Multiplication (left, right) -> execute left * execute right

let rec rightmostNumber =
    function
    | Number num -> num
    | Addition (_, right) -> rightmostNumber right
    | Multiplication (_, right) -> rightmostNumber right

let rec leftmostNumber =
    function
    | Number num -> num
    | Addition (left, _) -> leftmostNumber left
    | Multiplication (left, _) -> leftmostNumber left

let rec executeAdditionFirst =
    function
    | Number num -> num
    | Addition (left, right) -> execute left + execute right
    | Multiplication (left, right) -> execute left * execute right
  
let complexExpression left op right =
    match op with
    | '+' -> Addition (left, right)
    | _ -> Multiplication (left, right)

let pNumber = pint64 |>> Number

let inSpaces = between spaces spaces

let pSimpleExpression, pSimpleExpressionImpl = createParserForwardedToRef ()

let pSimpleSubExpression : Parser<Expression, unit> =
    pNumber <|> between (pchar '(') (pchar ')') pSimpleExpression

let pSimplePartialExpression = (pchar '+' <|> pchar '*') .>>. (pSimpleSubExpression |> inSpaces)

pSimpleExpressionImpl :=
    (pSimpleSubExpression |> inSpaces) .>>. many pSimplePartialExpression
    |>> (fun (first, others) -> others |> List.fold (fun acc (op, expr) -> complexExpression acc op expr) first)

let pExpression, pExpressionImpl = createParserForwardedToRef ()

let pSubExpression : Parser<Expression, unit> =
    pint64 |>> Number <|> between (pchar '(') (pchar ')') pExpression

let pAddition =
    (pSubExpression |> inSpaces) .>>. (many (pchar '+' >>. (pSubExpression |> inSpaces)))
    |>> (fun (first, others) -> others |> List.fold (fun acc expr -> complexExpression acc '+' expr) first)

pExpressionImpl :=
    (pAddition |> inSpaces) .>>. (many (pchar '*' >>. (pAddition |> inSpaces)))
    |>> (fun (first, others) -> others |> List.fold (fun acc expr -> complexExpression acc '*' expr) first)

let parseSimpleExpression str =
    match run pSimpleExpression str with
    | Success (result, _, _) -> Some result
    | _ -> None

let parseExpression str =
    match run pExpression str with
    | Success (result, _, _) -> Some result
    | _ -> None

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = file |> File.ReadAllLines |> List.ofArray

        input
        |> List.choose parseSimpleExpression
        |> List.map execute
        |> List.sum
        |> printfn "%d"

        input
        |> List.choose parseExpression
        |> List.map execute
        |> List.sum
        |> printfn "%d"

        0
    | _ ->
        printfn "Usage: Aoc.Day18 file"
        1
