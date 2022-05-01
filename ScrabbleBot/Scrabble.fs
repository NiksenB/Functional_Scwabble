﻿namespace Scwabble

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.Dictionary
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
        coordMap      : Map<coord, (uint32 * (char * int))>
        anchorLists   : List<coord * (uint32 * (char * int))> * List<coord * (uint32 * (char * int))>
        crossCheck    : Map<coord, Set<char>>
    }
    
    type coordTile = coord * (uint32 * (char *int))
    let mkState b d np pn pt f p h cm al cc = {board = b; dict = d;  numOfPlayers = np; playerNumber = pn; playerTurn = pt; forfeited = f; points = p; hand = h; coordMap = cm; anchorLists = al; crossCheck = cc }

    let board st         = st.board
    let dict st          = st.dict
    let numOfPlayers st  = st.numOfPlayers
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let forfeited st     = st.forfeited 
    let points st        = st.points
    let hand st          = st.hand
    let coordMap st      = st.coordMap
    let anchorLists st          = st.anchorLists
    let crossCheck st = st.crossCheck
    


module Scrabble =
    open System.Threading
    
    //TODO below takes a million years, we should rewrite to account for coordmap because this doesnt work
     let findMoveISBADDONTUSEONLYFORLOOKING (dict : Dict) (state : State.state) (ms : List<coord * (uint32 * (char *int))>) (pc : Map<uint32, tile>) =
                     //find valide ord ud fra de brikker vi har
                     let hand = state.hand
                     let rec loopthroughhand (hand : MultiSet.MultiSet<uint32>) acc ( (word : string), (nums : uint32 list)) =
                         if MultiSet.isEmpty hand
                         then acc
                         else
                         let list = MultiSet.toList hand
                         List.fold (fun acc x ->
                             let beh = Map.find x pc //dette er en tile og bogstav skal derfor trækkes ud herfra
                             let char = fst ((Set.toList beh)[0])
                             let validWord = step char dict //erstat 'b' med char når lortet virker //TODO 
                             let newWordNums = (word.Insert(-1, char.ToString()), nums@[x]) //wow 
                          
                             if validWord.IsSome
                             then
                                if fst validWord.Value
                                then loopthroughhand (MultiSet.removeSingle x hand) (acc@[newWordNums]) newWordNums
                                else loopthroughhand (MultiSet.removeSingle x hand) acc newWordNums
                             else
                                 acc      
                             ) [] list
          

      
                     loopthroughhand hand [] ("", [])  


    // let rec tryBuild dict coordmap hand pieces start = 


    // let rec goFind dict coordMap hand pieces coordCounter =
        
    //     if coordCounter = -1
    //         then
    //             if (tryBuild dict coordmap hand pieces (0,0)).isEmpty
    //             then
    //                 if coordMap.isEmpty then failwith "u gotta pass bro"
    //     else 
    //         if (tryBuild dict coordmap hand pieces (Map.keys (coordMap)).[coordCounter] |> fst) |> Map.isEmpty
    //         then goFind dict coordMap hand pieces (coordCounter+1)

    // let findMove (dict : Dict) (state : State.state) (pieces : Map<uint32, tile>) =
    //     goFind dict state.coordMap state.hand pieces -1 


    let hasNotLeftNeighbor ((x,y) : coord) coordMap = 
        not (Map.containsKey (x-1,y) coordMap)
    
    let hasNotUpNeighbor ((x,y) : coord) coordMap =
        not (Map.containsKey (x,y-1) coordMap)
    
    let hasNotDownNeighbor ((x,y) : coord) coordMap =
        not (Map.containsKey (x,y+1) coordMap)
        
    let getNextUpCoord coord=
        (fst coord, snd coord - 1)
    
    let getNextDownCoord coord=
        (fst coord, snd coord + 1) 
    let updateAnchors (coordMap : Map<coord, (uint32 * (char * int))>) = 
        Map.fold (fun (anchorListHorizontal, anchorListVertical) key value ->
            match ( hasNotLeftNeighbor key coordMap, hasNotUpNeighbor key coordMap) with
                |(true,true) -> ((key,value) :: anchorListHorizontal, (key,value) :: anchorListVertical)
                |(true,false) ->  ((key,value) :: anchorListHorizontal, anchorListVertical)
                |(false, true) -> (anchorListHorizontal, (key,value) :: anchorListVertical)
                | _ -> (anchorListHorizontal,anchorListVertical)
           ) (List.empty, List.Empty) coordMap
    
    let crossCheckUpAndUp crossCheckRules coord brikken coordmap newMoves =
        match (hasNotUpNeighbor coord newMoves, hasNotUpNeighbor (getNextUpCoord coord) newMoves) with
            |(true, false) ->
                // TODO this is where theres i both a neighboor up and down, we need to go up all the way first
                crossCheckRules
                //above is placeholder
            |(true, true) ->
                //TODO this is where we have no up up neighboor and we only need to look down.
                crossCheckRules
                // above is placeholder
            | _ -> crossCheckRules
    
    let crossCheckDownAndDown crossCheckRules coord brikken coordMap newMoves =
        match (hasNotDownNeighbor coord newMoves, hasNotDownNeighbor (getNextDownCoord coord) newMoves) with 
            | (true, false)  ->
                //this should be checked by the match above
                crossCheckRules
            | (true, true) ->
              // this is where there is two free spaces below and therefore not found by the match above,
              // TODO we need to look up the word to find what it says so we can place letters
               crossCheckRules
            | _ -> crossCheckRules
    
    let rec updateCrossChecks (newMoves : Map<coord, (uint32 * (char * int))>) coordmap =
        Map.fold (fun acc key value ->
            // this checks if above is free and if above above is free
            let newAcc = crossCheckUpAndUp acc key value coordmap newMoves
            
            // this finds the one where there is two down free
            crossCheckDownAndDown newAcc key value coordmap newMoves
            
            //TODO continure this trend but look for crosschecks on words that go vertical. 
            acc) list.Empty newMoves
            // todo update the state with the new rules 
        
    let legalMove (x: State.coordTile) =
        // TODO this should exist for ease of use
        x
    
    let getNextRightCoord coord =
        (fst coord+1, snd coord)
    
    
    let rec findWord (coord, (id , (ch , point))) currentWord (st : State.state) (dict : Dict) (haveAddedOwnLetter : bool) (hand : MultiSet<uint32>) (pieces : Map<uint32, tile>) coordFun =
            let isOccupiedRight = Map.containsKey (coordFun coord) st.coordMap
            if not isOccupiedRight
            then
                let nextDict = step ch dict
                if Option.isSome nextDict
                then
                    let isValidWord = fst (nextDict.Value)
                    if isValidWord && haveAddedOwnLetter
                    then
                          (true,snd (currentWord))
                    else
                          fold (fun acc id' amountOfElements->
                              if fst acc
                              then acc
                              else
                                  //find char der hører til id
                                  //kald findwordright med det char, med hand - char
                                  //TODO tag højde for crosschecks, ny liste det er intersectede med crosscheck uden for looepet

                                  let tile = Map.find id' pieces
                                  let ch' = fst ((Set.toList tile)[0]) //denne tile kan være 1 bogstav eller 26 bogstaver
                                  //TODO tag højde for brik der kan være alle bogstaver
                                  let point' = snd ((Set.toList tile)[0])
                                  let coord' = coordFun coord
                                  let dict' = snd (nextDict.Value)
                                  let currentWord'  = (false, snd currentWord@[(coord', (id' , (ch' , point')))] )
                                  let newMultiSet = removeSingle id' hand 
                                  
                                  findWord (coord', (id' , (ch' , point'))) currentWord' st dict' true newMultiSet pieces coordFun
                                  
                          ) (false, List.Empty) hand

                else
                    (false, snd (currentWord))
            else
                let letter = Map.find (fst coord+1, snd coord) st.coordMap
                let id' = (fst letter)
                let dict' = snd ((step ch dict).Value)
                let tile = Map.find id' pieces
                let point' = snd (snd letter)
                let coord' = coordFun coord
                let ch' = (fst (snd (letter)))
                
                findWord (coord', (id', (ch', point'))) currentWord st dict' true hand pieces coordFun
            
    let findMove anchorList (st : State.state) =       
        //For each move right call with get nextrightcoord.
        //List.fold (fun acc x -> findWordRight x acc st) List.Empty anchorList
       
    
            
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
    
    
    let updateMap oldmap message= List.fold (fun newmap (coord, brik) -> Map.add coord brik newmap) oldmap message
    
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
        
                let playedTiles = List.map (fun x -> (snd x) |> fun y -> ((fst y), (uint32) 1)) ms
                let handRemoveOld = MultiSet.subtract st.hand (setListToHand playedTiles)
                let handAddNew = MultiSet.sum handRemoveOld (setListToHand newPieces)
                let coordMap' = (updateMap st.coordMap ms) 
                let st' =   { st with
                                playerTurn = (getNextPlayerTurn st)
                                points = st.points + points
                                hand = handAddNew
                                coordMap = coordMap'
                                anchorLists = updateAnchors coordMap'
                }                                        
                
                forcePrint("Your hand: " + st'.hand.ToString())
                forcePrint("The board: " + st'.coordMap.ToString())
                forcePrint("Your player number: " + st'.playerNumber.ToString() + "\n\n")
                forcePrint("Your state: " + st'.ToString() + "\n\n")
                forcePrint("Next player: " + st'.playerTurn.ToString() + "\n\n")
                forcePrint("Your points: " + st'.points.ToString() + "\n\n")
                forcePrint(" ----- ----- ----- ----- -----")
                
                aux st'

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let coordMap' = (updateMap st.coordMap ms)              
                let st' = {st with
                            playerTurn = (getNextPlayerTurn st);
                            coordMap = coordMap'
                            anchorLists = (updateAnchors coordMap')
                           } 
                
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

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn Set.empty 0 handSet Map.empty (List.empty, List.Empty)) Map.empty
        
    
    
    
    
    
     
            

    
    
