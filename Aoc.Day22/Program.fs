open System
open System.IO

open FSharpPlus
open FSharpPlus.Data

type Card = Card of int

type Player = {
    Name : string
    Deck : Card list
}

type RecursiveGameState = {
    EncounteredDecks : (Card list * Card list) list
}

let stringToInt (str : string) =
    match str |> Int32.TryParse with
    | true, num -> Some num
    | _ -> None

let tryTail = function [] -> None | _ :: tail -> Some tail

let parsePlayer (strs : string list) =
    match strs with
    | [] -> None
    | head :: tail -> Some { Name = head.[..head.Length - 2]; Deck = tail |> List.choose stringToInt |> List.map Card }

let parseInput file =
    let lines = file |> File.ReadAllLines |> List.ofArray

    monad {
        let! index = lines |> List.tryFindIndex ((=) "")
        let player1, player2 = lines |> List.splitAt index
        let! player1 = player1 |> parsePlayer

        let! player2 = player2 |> tryTail
        let! player2 = player2 |> parsePlayer
        return player1, player2
    }

let getNextPlayerState player1 player2 head1 head2 tail1 tail2 player1Won =
    if player1Won then
        { player1 with Deck = tail1 @ [ head1; head2 ] }, { player2 with Deck = tail2 }
    else
        { player1 with Deck = tail1 }, { player2 with Deck = tail2 @ [ head2; head1 ]}

let rec playCombat (player1, player2) =
    match player1.Deck, player2.Deck with
    | [], _ -> player2
    | _, [] -> player1
    | head1 :: tail1, head2 :: tail2 ->
        getNextPlayerState player1 player2 head1 head2 tail1 tail2 (head1 > head2)
        |> playCombat

let initialState = { EncounteredDecks = [] }

let rec playRecursiveCombat (player1, player2) = monad {
    let! gameState = State.get

    if gameState.EncounteredDecks |> List.contains (player1.Deck, player2.Deck) then
        return player1
    else
        match player1.Deck, player2.Deck with
        | [], _ -> return player2
        | _, [] -> return player1
        | head1 :: tail1, head2 :: tail2 ->
            let newGameState = {
                gameState with EncounteredDecks = (player1.Deck, player2.Deck) :: gameState.EncounteredDecks
            }
            
            let (Card head1Value) = head1
            let (Card head2Value) = head2

            let tail1Length = tail1 |> List.length
            let tail2Length = tail2 |> List.length

            if tail1Length >= head1Value && tail2Length >= head2Value then
                let newPlayer1 = { player1 with Deck = tail1 |> List.take head1Value }
                let newPlayer2 = { player2 with Deck = tail2 |> List.take head2Value }
                
                do! State.put initialState
                let! winningPlayer = playRecursiveCombat (newPlayer1, newPlayer2)
                do! State.put newGameState

                return!
                    getNextPlayerState player1 player2 head1 head2 tail1 tail2 (winningPlayer.Name = player1.Name)
                    |> playRecursiveCombat
            else
                let newPlayer1, newPlayer2 = getNextPlayerState player1 player2 head1 head2 tail1 tail2 (head1 > head2)

                do! State.put newGameState

                return! (newPlayer1, newPlayer2) |> playRecursiveCombat
}

let calculateWinningScore player =
    let numCards = player.Deck |> List.length
    List.init numCards ((-) numCards)
    |> List.zip player.Deck
    |> List.map (fun (Card card, index) -> card * index)
    |> List.fold (+) 0

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        monad {
            let! player1, player2 = file |> parseInput

            let combatWinner = playCombat (player1, player2)
            let combatScore = combatWinner |> calculateWinningScore
            do printfn "%s: %d" combatWinner.Name combatScore
            
            let recursiveCombatWinner = State.eval (playRecursiveCombat (player1, player2)) initialState
            let recursiveCombatScore = recursiveCombatWinner |> calculateWinningScore
            do printfn "%s: %d" recursiveCombatWinner.Name recursiveCombatScore
        } |> ignore
        0
    | _ ->
        printfn "Usage: Aoc.Day22 file"
        1
