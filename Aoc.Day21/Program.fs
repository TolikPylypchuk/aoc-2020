open System
open System.IO

open FSharpPlus
open FSharpPlus.Data

open FParsec

type Ingredient = Ingredient of string
type Allergen = Allergen of string

type Food = {
    Ingredients : Ingredient list
    Allergens : Allergen list
}

type ProgramState = {
    AllIngredients : Ingredient list
    CheckedIngredients : Map<Ingredient, Allergen list>
}

let charsToString (chars : char list) =
    String.Concat(Array.ofList(chars))

let letters = many1Satisfy isLetter

let parseFood : Parser<Food, unit> =
    (many (letters .>> spaces)) .>>. (pstring "(contains " >>. (sepBy1 letters (pchar ',' .>> spaces)) .>> pchar ')') .>> eof
    |>> (fun (ingredients, allergens) -> {
        Ingredients = ingredients |> List.map Ingredient
        Allergens = allergens |> List.map Allergen
    })

let maybe parser str =
    match run parser str with
    | Success (result, _, _) -> Some result
    | _ -> None

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> List.choose (maybe parseFood)

let allingredients foods =
    [ for food in foods do yield! food.Ingredients ] |> List.distinct

let isPotentiallyDangerous foods (ingredient, allergen) =
    foods
    |> List.filter (fun food -> food.Allergens |> List.contains allergen)
    |> List.forall (fun food -> food.Ingredients |> List.contains ingredient)

let notContainedIn set item = set |> Set.contains item |> not

let determineSafeIngredients foods =
    let potentiallyDangerousIngredients =
        foods
        |> List.map (fun food -> List.allPairs food.Ingredients food.Allergens)
        |> List.concat
        |> List.distinct
        |> List.filter (isPotentiallyDangerous foods)
        |> List.map fst
        |> Set.ofList

    foods
    |> List.map (fun food -> food.Ingredients |> List.filter (notContainedIn potentiallyDangerousIngredients))
    |> List.concat
    |> List.distinct

let countOccurences foods ingredient =
    foods
    |> List.map (fun food -> food.Ingredients |> List.filter ((=) ingredient) |> List.length)
    |> List.sum

let ingredientsByAllergens foods =
    foods
    |> List.map (fun food -> food.Allergens |> List.map (fun allergen -> allergen, food.Ingredients |> Set.ofList))
    |> List.concat
    |> List.groupBy fst
    |> List.map (fun (allergen, items) -> allergen, items |> List.map snd |> List.reduce Set.intersect)

let getCanonicalDangerousIngredients safeIngredients foods =
    let safeIngredients = safeIngredients |> Set.ofList

    let foods =
        foods
        |> List.map (fun food ->
            { food with Ingredients = food.Ingredients |> List.filter (notContainedIn safeIngredients) })

    let knownItems, unknownItems =
        foods
        |> ingredientsByAllergens
        |> List.partition (fun (_, ingredients) -> ingredients |> Set.count = 1)

    let rec populateKnownItems knownItems unknownItems =
        let knownIngredients = knownItems |> List.map snd |> List.fold Set.union Set.empty

        let newKnownItems, newUnknownItems =
            unknownItems
            |> List.map (fun (allergen, ingredients) -> allergen, ingredients |> Set.filter (notContainedIn knownIngredients))
            |> List.partition (fun (_, ingredients) -> ingredients |> Set.count = 1)

        let newKnownItems = knownItems @ newKnownItems
        if newUnknownItems |> List.isEmpty
        then newKnownItems
        else populateKnownItems newKnownItems newUnknownItems

    populateKnownItems knownItems unknownItems
    |> List.sortBy fst
    |> List.map snd
    |> List.map Set.toList
    |> List.choose List.tryExactlyOne
    |> List.map (fun (Ingredient ingredient) -> ingredient)
    |> List.reduce (fun acc ingredient -> $"{acc},{ingredient}")

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let foods = file |> getInput

        let safeIngredients = foods |> determineSafeIngredients

        safeIngredients
        |> List.map (countOccurences foods)
        |> List.sum
        |> printfn "%d"

        foods |> getCanonicalDangerousIngredients safeIngredients |> printfn "%s"
        0
    | _ ->
        printfn "Usage: Aoc.Day21 file"
        1
