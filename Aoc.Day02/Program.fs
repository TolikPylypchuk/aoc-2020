open System.IO

open FParsec

type PasswordInfo = {
    FirstNumber : int
    SecondNumber : int
    Letter : char
    Password : string
}

let restOfInput : Parser<string, unit> =
    many1CharsTill anyChar (skipNewline <|> eof)

let pPasswordInfo : Parser<PasswordInfo, unit> =
    (pint32 .>> skipChar '-' .>>. pint32 .>> spaces1 .>>. anyChar .>> skipString ": " .>>. restOfInput)
    |>> (fun (((num1, num2), letter), pass) -> {
        FirstNumber = num1
        SecondNumber = num2
        Letter = letter
        Password = pass
    })

let parsePasswordInfo str =
    match run pPasswordInfo str with
    | Success (passwordInfo, _, _) -> Some passwordInfo
    | _ -> None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.map parsePasswordInfo
    |> List.choose id

let isValid1 password =
    let len =
        password.Password
        |> Seq.filter (fun letter -> letter = password.Letter)
        |> Seq.length
    len >= password.FirstNumber && len <= password.SecondNumber
    
let isValid2 password =
    let isFirstLetterValid = password.Password.[password.FirstNumber - 1] = password.Letter
    let isSecondLetterValid = password.Password.[password.SecondNumber - 1] = password.Letter
    isFirstLetterValid <> isSecondLetterValid
    
let countValidPasswords isValid passwords =
    passwords
    |> List.filter isValid
    |> List.length

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file
        input |> countValidPasswords isValid1 |> printfn "%d"
        input |> countValidPasswords isValid2 |> printfn "%d"
        0
    | _ ->
        printfn "Usage: Aoc.Day2 file"
        1
