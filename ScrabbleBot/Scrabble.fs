﻿namespace Scwabble

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        numOfPlayers  : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        forfeited     : Set<uint32>
        points        : int
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d np pn pt f p h = {board = b; dict = d;  numOfPlayers = np; playerNumber = pn; playerTurn = pt; forfeited = f; points = p; hand = h }

    let board st         = st.board
    let dict st          = st.dict
    let numOfPlayers st  = st.numOfPlayers
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let forfeited st     = st.forfeited 
    let points st        = st.points
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let setListToHand h = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty h

    let rec playerTurnHelper (np : uint32) (next : uint32) (pt : uint32) (f : uint32 Set)  =
        if next.Equals pt
            then failwith "It seems all other players have forfeited."
        else 
            if np >= next && not(Set.contains next f) //the next player is existing and active
                then next
            else if (np < next) //next player number exceeds the total number of players
                then playerTurnHelper np ((uint32) 0) pt f
            else if Set.contains next f //next player has forfeited
                then playerTurnHelper np (next + (uint32) 1) pt f
            else failwith "Unexpected error when finding next player."

    let getNextPlayerTurn (st : State.state) = 
        playerTurnHelper st.numOfPlayers (st.playerTurn + (uint32) 1) st.playerTurn st.forfeited

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            //TODO: Here below is where we should replace "input" from terminal with our algorithm function call
            //NOTE: Should only call the function to make a move, and "send cstream" if it is indeed our turn??
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                //TODO: Update some Map<coord, tile> or similar (which should be part of the state???), which tells us where tiles are now placed

                let playedTiles = List.map (fun x -> (snd x) |> fun y -> ((fst y), (uint32) 1)) ms
                let handRemoveOld = MultiSet.subtract st.hand (setListToHand playedTiles)
                let handAddNew = MultiSet.sum handRemoveOld (setListToHand newPieces)

                let st' =   State.mkState 
                                        st.board
                                        st.dict 
                                        st.numOfPlayers 
                                        st.playerNumber 
                                        (getNextPlayerTurn st)
                                        st.forfeited 
                                        (st.points + points) 
                                        handAddNew
                
                forcePrint("Your player number: " + st'.playerNumber.ToString() + "\n\n")
                forcePrint("Your state: " + st'.ToString() + "\n\n")
                forcePrint("Next player: " + st'.playerTurn.ToString() + "\n\n")
                forcePrint("Your points: " + st'.points.ToString() + "\n\n")
                
                aux st'

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                //TODO: Make sure the state also updates the board, not just player turn

                let st' = {st with playerTurn = (getNextPlayerTurn st)} 
                
                aux st'

            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)

                let st' = {st with playerTurn = (getNextPlayerTurn st)}
                 
                aux st'

            | RCM (CMGameOver (pointList)) ->
                for playerResults in pointList
                    do
                        forcePrint("Player " + (fst (playerResults)).ToString() + ": " + (snd (playerResults)).ToString() + "points.\n")
            
            | RCM (CMForfeit(pid)) ->
                //A player forfeits
                let st' = {st with forfeited = (Set.add pid st.forfeited)}  
                aux st'
            
            | RCM (CMPassed(pid)) -> 
                //Current player passes
                let currentSt = {st with playerTurn = pid} //this line might be redundant, I put it here just in case...
                let st' = {currentSt with playerTurn = (getNextPlayerTurn st)}
                aux st'
            
            | RCM (CMChangeSuccess(newTiles)) ->
                //You changed your tiles
                let st' = 
                    {st with 
                        hand = (MultiSet.sum st.hand (setListToHand newTiles));
                        playerTurn = getNextPlayerTurn st}
                aux st'

            | RCM (CMChange (pid, numTiles)) ->
                //Some other player changed their hand
                forcePrint("Player " + pid.ToString() + " changed " + numTiles.ToString() + " tiles.\n\n")
                let st' = {st with playerTurn = getNextPlayerTurn st}
                aux st'

            | RCM (CMTimeout (pid)) ->
                //SOme player timed out, which means they pass their turn
                let st' = {st with playerTurn = (getNextPlayerTurn {st with playerTurn = pid})} //again, making sure that we have the right "current" player stored might be redundant.
                aux st'

            | RCM a -> failwith (sprintf "RCM not implmented: %A" a)
            
            //TODO: Handle different Gameplay errors (see Scrabble.pdf)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn Set.empty 0 handSet)
        