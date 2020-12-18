open System
open System.IO
open System.Text.RegularExpressions

open FSharpPlus

type Height = Centimeters of int | Inches of int

type PassportInfo = {
    BirthYear : int option
    IssueYear : int option
    ExpirationYear : int option
    Height : Height option
    HairColor : string option
    EyeColor : string option
    PassportId : string option
    CountryId : string option
}

type ValidPassportInfo = {
    ValidBirthYear : int
    ValidIssueYear : int
    ValidExpirationYear : int
    ValidHeight : Height
    ValidHairColor : string
    ValidEyeColor : string
    ValidPassportId : string
    ValidCountryId : string option
}

type AccumulatedLine = Incomplete of string list | Complete of string

let (|Int|_|) (str : string) =
    match str |> Int32.TryParse with
    | true, result -> Some result
    | _ -> None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let (|BirthYear|_|) =
    function
    | Int num -> if num >= 1920 && num <= 2002 then Some num else None
    | _ -> None

let (|IssueYear|_|) =
    function
    | Int num -> if num >= 2010 && num <= 2020 then Some num else None
    | _ -> None

let (|ExpirationYear|_|) =
    function
    | Int num -> if num >= 2020 && num <= 2030 then Some num else None
    | _ -> None

let (|Height|_|) =
    function
    | Regex "^([0-9]+)cm$" [ Int num ] -> if num >= 150 && num <= 193 then Some <| Centimeters num else None
    | Regex "^([0-9]+)in$" [ Int num ] -> if num >= 59 && num <= 76 then Some <| Inches num else None
    | _ -> None

let (|HairColor|_|) str =
    match str with
    | Regex "^#([0-9a-f]){6}$" _ -> Some str
    | _ -> None

let (|EyeColor|_|) str =
    match str with
    | Regex "^amb|blu|brn|gry|grn|hzl|oth$" _ -> Some str
    | _ -> None

let (|PassportId|_|) str =
    match str with
    | Regex "^([0-9]){9}$" _ -> Some str
    | _ -> None

let emptyPassportInfo = {
    BirthYear = None
    IssueYear = None
    ExpirationYear = None
    Height = None
    HairColor = None
    EyeColor = None
    PassportId = None
    CountryId = None
}

let validatePassportInfo passportInfo = monad {
    let! birthYear = passportInfo.BirthYear
    let! issueYear = passportInfo.IssueYear
    let! expirationYear = passportInfo.ExpirationYear
    let! height = passportInfo.Height
    let! hairColor = passportInfo.HairColor
    let! eyeColor = passportInfo.EyeColor
    let! passportId = passportInfo.PassportId

    return {
        ValidBirthYear = birthYear
        ValidIssueYear = issueYear
        ValidExpirationYear = expirationYear
        ValidHeight = height
        ValidHairColor = hairColor
        ValidEyeColor = eyeColor
        ValidPassportId = passportId
        ValidCountryId = passportInfo.CountryId
    }
}

let parsePassportItem passportInfo str =
    match str |> String.split [ ":" ] |> List.ofSeq with
    | [ "byr"; BirthYear birthYear ] -> { passportInfo with BirthYear = Some birthYear }
    | [ "iyr"; IssueYear issueYear ] -> { passportInfo with IssueYear = Some issueYear }
    | [ "eyr"; ExpirationYear expirationYear ] -> { passportInfo with ExpirationYear = Some expirationYear }
    | [ "hgt"; Height height ] -> { passportInfo with Height = Some height }
    | [ "hcl"; HairColor hairColor ] -> { passportInfo with HairColor = Some hairColor }
    | [ "ecl"; EyeColor eyeColor ] -> { passportInfo with EyeColor = Some eyeColor }
    | [ "pid"; PassportId passportId ] -> { passportInfo with PassportId = Some passportId }
    | [ "cid"; countryId ] -> { passportInfo with CountryId = Some countryId }
    | _ -> passportInfo

let parsePassportInfo str =
    str
    |> String.split [ " " ]
    |> Seq.fold parsePassportItem emptyPassportInfo

let accumulateNonEmptyLines scannedLine line =
    match (scannedLine, line) with
    | (Incomplete [], "") -> Incomplete []
    | (Incomplete lines, "") -> lines |> List.reduce (flip <| sprintf "%s %s") |> Complete
    | (Incomplete lines, line) -> line :: lines |> Incomplete
    | (Complete _, "") -> Incomplete []
    | (Complete _, line) -> Incomplete [ line ]

let readAndReduceLines file =
    Seq.append (File.ReadLines(file)) (Seq.singleton "")
    |> Seq.scan accumulateNonEmptyLines (Incomplete [])
    |> Seq.choose (function Complete str -> Some str | _ -> None)

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        file
        |> readAndReduceLines
        |> Seq.map parsePassportInfo
        |> Seq.choose validatePassportInfo
        |> Seq.length
        |> printfn "%d"
        0
    | _ ->
        printfn "Usage: Aoc.Day4 file"
        1
