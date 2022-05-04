namespace Scwabble

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
        fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dict
        numOfPlayers  : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        forfeited     : Set<uint32>
        points        : int
        hand          : MultiSet.MultiSet<uint32>
        coordMap      : Map<coord, uint32 * (char * int)>
        anchorLists   : List<coord * (uint32 * (char * int))> * List<coord * (uint32 * (char * int))>
        crossCheck    : Map<coord, Set<char>>
    }
    
    type coordTile = coord * (uint32 * (char *int))

    let mkState b d np pn pt f p h cm al cc = 
        {board = b; dict = d;  numOfPlayers = np; playerNumber = pn; playerTurn = pt; forfeited = f; points = p; hand = h; coordMap = cm; anchorLists = al; crossCheck = cc }

    let board st         = st.board
    let dict st          = st.dict
    let numOfPlayers st  = st.numOfPlayers
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let forfeited st     = st.forfeited 
    let points st        = st.points
    let hand st          = st.hand
    let coordMap st      = st.coordMap
    let anchorLists st   = st.anchorLists
    let crossCheck st    = st.crossCheck
    


module Scrabble =
    open System.Threading
    
    //TODO below takes a million years, we should rewrite to account for coordmap because this doesnt work
    // let findMoveISBADDONTUSEONLYFORLOOKING (dict : Dict) (state : State.state) (ms : List<coord * (uint32 * (char *int))>) (pc : Map<uint32, tile>) =
    //                  //find valide ord ud fra de brikker vi har
    //                  let hand = state.hand
    //                  let rec loopthroughhand (hand : MultiSet.MultiSet<uint32>) acc ( (word : string), (nums : uint32 list)) =
    //                      if MultiSet.isEmpty hand
    //                      then acc
    //                      else
    //                      let list = MultiSet.toList hand
    //                      List.fold (fun acc x ->
    //                          let beh = Map.find x pc //dette er en tile og bogstav skal derfor trækkes ud herfra
    //                          let char = fst ((Set.toList beh)[0])
    //                          let validWord = step char dict //erstat 'b' med char når lortet virker //TODO 
    //                          let newWordNums = (word.Insert(-1, char.ToString()), nums@[x]) //wow 
                          
    //                          if validWord.IsSome
    //                          then
    //                             if fst validWord.Value
    //                             then loopthroughhand (MultiSet.removeSingle x hand) (acc@[newWordNums]) newWordNums
    //                             else loopthroughhand (MultiSet.removeSingle x hand) acc newWordNums
    //                          else
    //                              acc      
    //                          ) [] list
          
    //                  loopthroughhand hand [] ("", [])  


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

    let getNextRightCoord coord =
        (fst coord + 1, snd coord)

    let updateAnchors (coordMap : Map<coord, uint32 * (char * int)>) = 
        Map.fold (fun (anchorListHorizontal, anchorListVertical) key value ->
            match ( hasNotLeftNeighbor key coordMap, hasNotUpNeighbor key coordMap) with
                |true,true -> ((key,value) :: anchorListHorizontal, (key,value) :: anchorListVertical)
                |true,false ->  ((key,value) :: anchorListHorizontal, anchorListVertical)
                |false, true -> (anchorListHorizontal, (key,value) :: anchorListVertical)
                | _ -> (anchorListHorizontal,anchorListVertical)
           ) (List.empty, List.Empty) coordMap
    
    let rec goToStartOfWordBelow (x, y) acc coordmap=
        if Map.containsKey (x,y+1) coordmap
        then goToStartOfWordBelow (x,(y+1)) ([Map.find (x,(y+1)) coordmap] @ acc) coordmap
        else acc
    //TODO these two functions could be the same, if there was a function given with as parameter that changed the coord
    let rec goToStartOfWordAbove (x, y) acc coordmap=
        if Map.containsKey (x,y-1) coordmap
        then goToStartOfWordAbove (x,(y-1)) ([Map.find (x,(y-1)) coordmap] @ acc) coordmap
        else acc
    let rec stepThruWord wordSoFar (dict : Option<bool * Dict>) =
        match wordSoFar with
        |[] -> dict
        | (_ , (ch , _)) :: wordSoFar ->
            let dict' = step ch (snd dict.Value)
            stepThruWord wordSoFar dict'
    
    let lookUpWithStringStartingAtEveryLetterOfAlphabet word dict =
        let alphabet = "abcdefghijklmnopqrstuvwxyz"
        let chars = alphabet.ToCharArray() |> List.ofArray
        let validLetters =
            List.fold (fun acc char ->
            let dict' = step char dict
            if dict'.IsSome
            then                
                let wordExists = lookup word (snd dict'.Value)
                if wordExists then acc @ [char] else acc
            else
                acc
            ) [] chars
        Set.ofList validLetters 
    
    let crossCheckUpAndUp (crossCheckRules : Map<coord, Set<char>>) brik (coordmap : Map<coord, uint32 * (char * int)>) (state : State.state) =
        let coord = fst brik
        //Before this check coordmap has been updated with the new moves as well, very important this is done.
        let exactlyOneFreeAbove = (true, false)
        let twoFreeAbove = (true, true)
        
        match (hasNotUpNeighbor coord coordmap, hasNotUpNeighbor (getNextUpCoord coord) coordmap) with
            |true, false ->
                // this is where theres emptytile both a neighboor up and down, we need to go up all the way first
                let emptyTile = getNextUpCoord coord      
                let wordBelowAsList = goToStartOfWordBelow emptyTile List.Empty coordmap
                let wordBelow = List.fold (fun acc (_, (ch, _)) -> acc+ch.ToString()) "" wordBelowAsList
                let wordAbove = goToStartOfWordAbove emptyTile List.Empty coordmap
                let dictFromWordAbove = stepThruWord wordAbove (Option.Some (false, state.dict))
                if dictFromWordAbove.IsSome 
                then
                    let dictFromAboveNotOption = snd dictFromWordAbove.Value
                    let validLetters = lookUpWithStringStartingAtEveryLetterOfAlphabet wordBelow dictFromAboveNotOption                    
                    //TODO, if something already has rules for this tiles, we may need to intersect, but this however is the first rule called, so we may not need.
                    Map.add emptyTile validLetters crossCheckRules
                else
                    Map.add  (getNextUpCoord coord ) Set.empty crossCheckRules
                
            |true, true ->
                //No upstairs word, we only need to looks down
                let emptyTile = getNextUpCoord coord
                let wordBelowAsList = goToStartOfWordBelow emptyTile List.Empty coordmap
                let wordBelow = List.fold (fun acc (_, (ch, _)) -> acc+ch.ToString()) "" wordBelowAsList
                let dict =  state.dict
                let validLetters = lookUpWithStringStartingAtEveryLetterOfAlphabet wordBelow dict                    
                Map.add emptyTile validLetters crossCheckRules
                
            | _ -> crossCheckRules
    
    let crossCheckDownAndDown crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        //the crosscheckDownDown only needs to find if the two below are free, as the crosscheckupandup will have found any empty tile between to vertical words.
        let coord = fst brik
        match (hasNotDownNeighbor coord coordMap, hasNotDownNeighbor (getNextDownCoord coord) coordMap) with 
            | true, true ->
              // this is where there is two free spaces below and therefore not found by the match above,
              // TODO we need to look up the word to find what it says so we can place letters
               crossCheckRules
            | _ -> crossCheckRules
    
    let rec updateCrossChecks (newMoves : (coord * (uint32 * (char * int))) list) coordMap (crossCheck : Map<coord, Set<char>>) state=
        
        List.fold (fun acc brik ->
            // this checks if above is free and if above above is free
            let newAcc = crossCheckUpAndUp acc brik coordMap  state
            
            // this finds the one where there is two down free
            let newAcc2 = crossCheckDownAndDown newAcc brik coordMap  state
            
            //TODO continure this trend but look for crosschecks on words that go vertical. 
            newAcc2
            
            ) crossCheck newMoves



            // todo update the state with the new rules     
    
    let rec findWord (coord, (id , (ch , point))) currentWord (st : State.state) (dict : Dict) (haveAddedOwnLetter : bool) (hand : MultiSet<uint32>) (pieces : Map<uint32, tile>) coordFun =
        //TODO coordfun here skal transforme coord til et step til højre, så den er nok ikke behov for den i findword men vi skal lave en til nedenunder her
        let isOccupiedNextToMe = Map.containsKey (coordFun coord) st.coordMap
        
        if not isOccupiedNextToMe
        then  
            let nextDict = step ch dict
            if Option.isSome nextDict
            then
                let isValidWord = fst nextDict.Value
                if isValidWord && haveAddedOwnLetter
                then
                    (true,snd currentWord)
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
                            let dict' = snd nextDict.Value
                            let currentWord'  = (false, snd currentWord@[(coord', (id' , (ch' , point')))] )
                            let newMultiSet = removeSingle id' hand 
                            
                            findWord (coord', (id' , (ch' , point'))) currentWord' st dict' true newMultiSet pieces coordFun
                            
                    ) (false, List.Empty) hand
            else
                (false, snd currentWord)
        else
            let letter = Map.find (fst coord+1, snd coord) st.coordMap
            let id' = (fst letter)
            let dict' = snd (step ch dict).Value
            let tile = Map.find id' pieces
            let point' = snd (snd letter)
            let coord' = coordFun coord
            let ch' = (fst (snd letter))
            
            findWord (coord', (id', (ch', point'))) currentWord st dict' true hand pieces coordFun

    
    let findOneMove (st : State.state) pieces =
        //TODO her skal være to folds, en vi mangler en hvor vi går ned af også. 
               
        List.fold (fun acc x  -> 
            if fst acc 
            then acc
            else
                findWord (x) (false, List.Empty) st st.dict false st.hand pieces getNextRightCoord
        ) (false,List.Empty) (fst st.anchorLists) 
        
           
    let setListToHand h = List.fold (fun acc (x, k) -> add x k acc) empty h

    let rec playerTurnHelper (np : uint32) (next : uint32) (pt : uint32) (f : uint32 Set)  =
        if next.Equals pt
            then failwith "It seems all other players have forfeited."
        else 
            if np >= next && not(Set.contains next f) //the next player is existing and active
                then next
            else if (np < next) //next player number exceeds the total number of players
                then playerTurnHelper np (uint32 0) pt f
            else if Set.contains next f //next player has forfeited
                then playerTurnHelper np (next + uint32 1) pt f
            else failwith "Unexpected error when finding next player."

    let getNextPlayerTurn (st : State.state) = 
        playerTurnHelper st.numOfPlayers (st.playerTurn + uint32 1) st.playerTurn st.forfeited
    
    let updateMap oldmap message= List.fold (fun newmap (coord, brik) -> Map.add coord brik newmap) oldmap message
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            //Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            

            //TODO should we somehow check that st.playerTurn = st.playerNumber before trying to play?
            
            Print.printHand pieces (State.hand st)
            let theMoveWellTryToMake : bool * list<coord * (uint32 * (char * int))> = findOneMove st pieces
            if fst theMoveWellTryToMake
            then 
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) theMoveWellTryToMake) // keep the debug lines. They are useful.
                send cstream (SMPlay (snd theMoveWellTryToMake))
            else send cstream (SMPass)
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) theMoveWellTryToMake) // keep the debug lines. They are useful.



            //let input =  System.Console.ReadLine()
            //let move = RegEx.parseMove input

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //send cstream (SMPlay move)

            // let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
        
                let playedTiles = List.map (fun x -> (snd x) |> fun y -> ((fst y), uint32 1)) ms
                let handRemoveOld = subtract st.hand (setListToHand playedTiles)
                let handAddNew = sum handRemoveOld (setListToHand newPieces)
                let coordMap' = (updateMap st.coordMap ms) 
                let crossCheck' = updateCrossChecks ms coordMap' st.crossCheck st

                let st' =   { st with
                                playerTurn = (getNextPlayerTurn st)
                                points = st.points + points
                                hand = handAddNew
                                coordMap = coordMap'
                                anchorLists = updateAnchors coordMap'
                                crossCheck = crossCheck'
                }                                        
                
                forcePrint("Your hand: " + st'.hand.ToString())
                forcePrint("The board: " + st'.coordMap.ToString())
                forcePrint("Your player number: " + st'.playerNumber.ToString() + "\n\n")
                forcePrint("Your state: " + st'.ToString() + "\n\n")
                forcePrint("Next player: " + st'.playerTurn.ToString() + "\n\n")
                forcePrint("Your points: " + st'.points.ToString() + "\n\n")
                forcePrint("Crosscreck: " + st'.crossCheck.ToString() + "\n\n")
                forcePrint(" ----- ----- ----- ----- -----")
                
                aux st'

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let coordMap' = (updateMap st.coordMap ms)       
                let crossCheck' = updateCrossChecks ms coordMap' st.crossCheck st    

                let st' = {st with
                            playerTurn = (getNextPlayerTurn st);
                            coordMap = coordMap'
                            anchorLists = (updateAnchors coordMap')
                            crossCheck = crossCheck'
                           } 
                
                aux st'

            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = {st with playerTurn = (getNextPlayerTurn st)}
                 
                aux st'

            | RCM (CMGameOver pointList) ->
                for playerResults in pointList
                    do
                        forcePrint("Player " + (fst playerResults).ToString() + ": " + (snd playerResults).ToString() + "points.\n")
            
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
                        hand = (sum st.hand (setListToHand newTiles));
                        playerTurn = getNextPlayerTurn st}
                aux st'

            | RCM (CMChange (pid, numTiles)) ->
                //Some other player changed their hand
                forcePrint("Player " + pid.ToString() + " changed " + numTiles.ToString() + " tiles.\n\n")
                let st' = {st with playerTurn = getNextPlayerTurn st}
                aux st'

            | RCM (CMTimeout pid) ->
                //Some player timed out, which means they pass their turn
                let st' = {st with playerTurn = (getNextPlayerTurn {st with playerTurn = pid})} //again, making sure that we have the right "current" player stored might be redundant.
                aux st'

            | RCM a -> failwith (sprintf "RCM not implmented: %A" a)
        
        
            //TODO Handle a few of the different Gameplay errors? (see Scrabble.pdf)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
    
    
        aux st



    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dict) 
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
                
        let handSet = List.fold (fun acc (x, k) -> add x k acc) empty hand

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn Set.empty 0 handSet Map.empty (List.empty, List.Empty) Map.empty) 
    
    
    
     
            

    
    
