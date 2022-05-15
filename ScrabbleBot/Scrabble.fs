namespace Scwabble

open System.Diagnostics
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
                | _ ->
                    debugPrint("lol shud never happn")
                    failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        fold (fun _ x i -> debugPrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    
    type crossChecks = {
        checkForHorizontalWords : Map<coord, Set<char>>
        checkForVerticalWords   : Map<coord, Set<char>>
    }
    
    type anchors = {
        anchorsForHorizontalWords : List<coord * (uint32 * (char * int))>
        anchorsForVerticalWords : List<coord * (uint32 * (char * int))>
        
    }
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
        anchorLists   : anchors
        crossChecks   : crossChecks
        piecesLeft    : int
        haveJustSwappedTiles : bool
    }
    
    
    
    type coordTile = coord * (uint32 * (char *int))

    let mkCrossChekcs h v = {checkForHorizontalWords = h; checkForVerticalWords = v;}
    
    let mkAnchors h v = {anchorsForHorizontalWords = h; anchorsForVerticalWords =v; }
    let mkState b d np (pn:uint32) pt f p h cm al cc =
        let tiles = (100-((7) * (int) np))
        {board = b; dict = d;  numOfPlayers = np; playerNumber = pn; playerTurn = pt; forfeited = f; points = p; hand = h; coordMap = cm; anchorLists = al; crossChecks = cc; piecesLeft = tiles; haveJustSwappedTiles = false;  }

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
    let crossChecks st   = st.crossChecks
    let piecesLeft st    = st.piecesLeft
    


module Scrabble =
    open System.Threading
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let getNextUpCoord (coord : coord) =
        (fst coord, snd coord - 1)
    
    let getNextDownCoord (coord : coord) =
        (fst coord, snd coord + 1) 

    let getNextRightCoord (coord : coord) =
        (fst coord + 1, snd coord)
    let getNexLeftCoord (coord : coord) =
        (fst coord - 1, snd coord)
    
    let hasNotLeftNeighbor coord coordMap = 
        not (Map.containsKey (getNexLeftCoord coord) coordMap)
    
    let hasNotUpNeighbor coord coordMap =        
        not (Map.containsKey (getNextUpCoord coord) coordMap)
    
    let hasNotDownNeighbor coord coordMap =
        not (Map.containsKey (getNextDownCoord coord) coordMap)
    
    let hasNotRightNeighbor coord coordMap =
        not (Map.containsKey (getNextRightCoord coord) coordMap)

    let updateAnchors (coordMap : Map<coord, uint32 * (char * int)>) = 
        let (anchorListHorizontal, anchorListVertical) =
            Map.fold (fun (anchorListHorizontal, anchorListVertical) key value ->
            match ( hasNotLeftNeighbor key coordMap, hasNotUpNeighbor key coordMap) with
                |true,true ->                    
                    let hori' =  (key,value) :: anchorListHorizontal 
                    let verti' = (key,value) :: anchorListVertical
                    //it gets the coordinates just left and above of the stuff put on the map, if they are free
                    
                    if (hasNotLeftNeighbor  (getNexLeftCoord (getNexLeftCoord key)) coordMap) && (hasNotUpNeighbor (getNextUpCoord (getNextUpCoord key)) coordMap)
                    then                       
                            ((getNexLeftCoord key,value) :: hori',  (getNextUpCoord key,value) :: verti')
                    else (hori', verti')
                    
                    
                   
                |true,false ->
                    let hori' = (key,value) :: anchorListHorizontal
                    
                    (hori', anchorListVertical)
            
                |false, true ->                    
                    let verti' = (key,value) :: anchorListVertical
                    
                    (anchorListHorizontal, verti')
                
                | _ -> (anchorListHorizontal,anchorListVertical)
            )(List.empty, List.Empty) coordMap
        State.mkAnchors anchorListHorizontal anchorListVertical
    
    
    let rec goToStartOfWord ((x,y): coord) acc coordMap coordFun =
        let coord = coordFun (x,y)
        if Map.containsKey coord coordMap
        then goToStartOfWord coord ([Map.find coord coordMap] @ acc) coordMap coordFun
        else acc
    
    let rec goFromStartOfWord (x,y) acc coordMap coordFun =
        let coord = coordFun (x,y)
        if Map.containsKey coord coordMap
        then goToStartOfWord coord ( acc @ [Map.find coord coordMap]) coordMap coordFun
        else acc
        
    let goToStartOfWordBelow (x, y) coordMap =
        goFromStartOfWord (x,y) List.Empty coordMap getNextDownCoord
    
    let goToStartOfWordAbove (x, y) coordMap =
        goToStartOfWord (x,y) List.Empty coordMap getNextUpCoord
    
    let goToStartOfWordLeft (x, y) coordMap =
        goToStartOfWord (x,y) List.Empty coordMap getNexLeftCoord
    
    let goToStartOfWordRight (x, y) coordMap =
        goFromStartOfWord (x,y) List.Empty coordMap getNextRightCoord
        
    let rec stepThruWord wordSoFar (dict : Option<bool * Dict>) =
        match wordSoFar with
        |[] -> dict
        | (_ , (ch , _)) :: wordSoFar ->
            let dict' = step ch (snd dict.Value)
            stepThruWord wordSoFar dict'
    
    let lookUpWithStringStartingAtEveryLetterOfAlphabet word dict =        
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
        
    let PossibleEndingsToWordWithEveryLetterOfAlphabet dict =
            let chars = alphabet.ToCharArray() |> List.ofArray
            let validLetters =
                List.fold (fun acc char ->
                let dict' = step char dict
                if dict'.IsSome
                then                
                    if (fst dict'.Value) then acc @ [char] else acc
                else
                    acc
                ) [] chars
            Set.ofList validLetters 
    
    let rec crossCheckGenericTwoBefore
     (crossCheckRules : Map<coord, Set<char>>) brik (coordMap : Map<coord, uint32 * (char * int)>) (state : State.state) checkNeighborIsFree nextNeighBorCoord goToStartOfWordBefore goToStartOfWordAfter =
        let coord = fst brik
        //Before this check coordmap has been updated with the new moves as well, very important this is done.
        let exactlyOneFreeAbove = (true, false)
        let twoFreeAbove = (true, true)
        
        match (checkNeighborIsFree coord coordMap, checkNeighborIsFree (nextNeighBorCoord coord) coordMap) with //getNexTUpCord, Hasupneightbor
            |true, false ->
                let emptyTile = nextNeighBorCoord coord      
                let wordBelowAsList = goToStartOfWordAfter emptyTile coordMap 
                let wordBelow = List.fold (fun acc (_, (ch, _)) -> ch.ToString()+acc) "" wordBelowAsList
                let wordAbove = goToStartOfWordBefore emptyTile coordMap 
                let dictFromWordAbove = stepThruWord wordAbove (Option.Some (false, state.dict))
                if dictFromWordAbove.IsSome 
                then
                    let dictFromAboveNotOption = snd dictFromWordAbove.Value
                    let validLetters = lookUpWithStringStartingAtEveryLetterOfAlphabet wordBelow dictFromAboveNotOption                    
                    
                    Map.add emptyTile validLetters crossCheckRules
                else
                    Map.add  (nextNeighBorCoord coord ) Set.empty crossCheckRules                
            |true, true ->
                //No upstairs word, we only need to looks down
                let emptyTile = nextNeighBorCoord coord
                let wordBelowAsList = goToStartOfWordAfter emptyTile coordMap
                
                let wordBelow = List.fold (fun acc (_, (ch, _)) -> ch.ToString()+acc) "" wordBelowAsList
                
                let validLetters = lookUpWithStringStartingAtEveryLetterOfAlphabet wordBelow state.dict                    
                Map.add emptyTile validLetters crossCheckRules
            | (false, _) ->
                //I have a neighbor and possibly more.
                let notABrik = Map.find (nextNeighBorCoord coord) coordMap
                let brik' = ((nextNeighBorCoord coord), notABrik)
                crossCheckGenericTwoBefore crossCheckRules brik' coordMap state checkNeighborIsFree nextNeighBorCoord goToStartOfWordBefore goToStartOfWordAfter
            
            
    
    let crossCheckAfter (crossCheckRules : Map<coord, Set<char>>) brik (coordMap : Map<coord, uint32 * (char * int)>) (state : State.state) isNeighborFree nextNeighborCoord goToStartOfWordBefore goToStartOfWordAfter=
        //the crosscheckDownDown only needs to find if the two below are free, as the crosscheckupandup will have found any empty tile between to vertical words.
        let coord = fst brik
    

        match (isNeighborFree coord coordMap, isNeighborFree (nextNeighborCoord coord) coordMap) with 
            | true, true ->
              // this is where there is two free spaces below and therefore not found by the match above,
              let emptyTile = nextNeighborCoord coord 
              let wordAbove = goToStartOfWordBefore emptyTile coordMap
              
              let dictFromWordAbove = stepThruWord wordAbove (Option.Some (false, state.dict))
              
              if dictFromWordAbove.IsSome
              then
                    
                    let dictFromAboveNotOption = snd dictFromWordAbove.Value
                    let validLetters = PossibleEndingsToWordWithEveryLetterOfAlphabet dictFromAboveNotOption
                    Map.add emptyTile validLetters crossCheckRules
              else
                    Map.add  emptyTile Set.empty crossCheckRules
            | true, false ->
                let emptyTile = nextNeighborCoord coord
                
               
                let wordBefore = goToStartOfWordBefore emptyTile coordMap 
                let dictFromWordBefore = stepThruWord wordBefore (Option.Some (false, state.dict))
                
                let wordAfterAsList = goToStartOfWordAfter emptyTile coordMap 
                let wordAfter = List.fold (fun acc (_, (ch, _)) -> ch.ToString()+acc) "" wordAfterAsList
                
                if dictFromWordBefore.IsSome 
                then
                    let dictFromAboveNotOption = snd dictFromWordBefore.Value
                    let validLetters = lookUpWithStringStartingAtEveryLetterOfAlphabet wordAfter dictFromAboveNotOption                    
                    
                    Map.add emptyTile validLetters crossCheckRules
                else
                    Map.add  (nextNeighborCoord coord ) Set.empty crossCheckRules 
               
            | (false, _) ->
                //I have a neighbor and possibly more.
                let notABrik = Map.find (nextNeighborCoord coord) coordMap
                let brik' = ((nextNeighborCoord coord), notABrik)
                crossCheckGenericTwoBefore crossCheckRules brik' coordMap state isNeighborFree nextNeighborCoord goToStartOfWordBefore goToStartOfWordAfter    
            | _ -> crossCheckRules
            
    let crossCheckUpAndUp crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckGenericTwoBefore
         crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state hasNotUpNeighbor getNextUpCoord goToStartOfWordAbove goToStartOfWordBelow
    
    let crossCheckDownAndDown crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckAfter crossCheckRules brik  coordMap state hasNotDownNeighbor getNextDownCoord goToStartOfWordAbove goToStartOfWordBelow      
    let crossCheckLeftAndLeft crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckGenericTwoBefore
         crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state hasNotLeftNeighbor getNexLeftCoord goToStartOfWordLeft goToStartOfWordRight
    
    let crossCheckRightAndRight crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckAfter crossCheckRules brik  coordMap state hasNotRightNeighbor getNextRightCoord goToStartOfWordLeft goToStartOfWordRight
           
    let rec updateCrossChecks (newMoves : (coord * (uint32 * (char * int))) list) coordMap (st : State.state)=
        
        
        let rec runThroughNewTiles (upAndDown, leftAndRight) moves =
            match moves with
            | [] -> (upAndDown, leftAndRight)
            | brik :: brikker ->
                let upAndUp = crossCheckUpAndUp upAndDown brik coordMap  st
                // this finds the one where there is two down free
                
                let upAndDownResult = crossCheckDownAndDown upAndUp brik coordMap  st
                
                let leftAndLeft = crossCheckLeftAndLeft leftAndRight brik coordMap st
                
                let leftAndRightResult = crossCheckRightAndRight leftAndLeft brik coordMap st
                
                runThroughNewTiles (upAndDownResult, leftAndRightResult) brikker           
                
        let (upAndDown, leftAndRight) = runThroughNewTiles (st.crossChecks.checkForHorizontalWords, st.crossChecks.checkForVerticalWords) newMoves
        {st.crossChecks with checkForHorizontalWords = upAndDown; checkForVerticalWords = leftAndRight;}
        
    
    
    
    let rec findFirstWord (hand : MultiSet<uint32>) (dict : Dict) (pieces : Map<uint32, tile>) (result : bool * list<coord * (uint32 * (char * int))>) =
        if MultiSet.isEmpty hand || fst result
        then result
        else
            MultiSet.fold (fun acc id amount -> //Go through the hand
                let tile = Map.find id pieces
                
                Set.fold (fun accWithChar c -> //for each possible char value a tile can have, try build word
                    if fst accWithChar
                    then accWithChar
                    else 
                        let ch = fst c
                        let dictOption = step ch dict
                        let currentList = snd accWithChar
                        if dictOption.IsSome
                        then
                            let dict' = snd (dictOption.Value)
                            let amputatedHand = MultiSet.removeSingle id hand
                            let xAxisPlacement = snd accWithChar |> List.length
                            
                            let newList = currentList@[((0,xAxisPlacement),(id,c))]
                            if fst (dictOption.Value) && List.length newList >= 2
                            then
                                (true, newList)
                            else
                                let acc' = (false, newList)
                                let (b', word) = findFirstWord amputatedHand dict' pieces acc'
                                if b' then (b', word) else (b', snd acc)
                                
                        else //we're headed down a branch with no destination, skip this branch/combination
                            (false, snd acc)

                ) acc tile
            ) result hand
    
    let rec findAllPossibleWords coord (bestWord : ((coord * (uint32 * (char * int))) list) list) (currentAddedTiles : ((coord * (uint32 * (char * int))) list)) (st : State.state) (dict : Dict) (hand : MultiSet<uint32>) (pieces : Map<uint32, tile>) coordFun crossCheck =
       
        let isCoordOccupied = Map.containsKey coord st.coordMap
        
        if not isCoordOccupied
        then
            if isEmpty hand
            then
                (bestWord, list.Empty)
            else
                fold (fun (f, s) id _ ->
                    let tile = Map.find id pieces
                    Set.fold (fun accWithChar c ->
                        let ch' = fst c
                        if Map.containsKey coord crossCheck
                        then
                            if Set.contains ch' (Map.find coord crossCheck)
                            then
                                let dOption = step ch' dict
                                if dOption.IsSome
                                then
                                    let currentWord' = s@[(coord, (id, c))]
                                    let dictValue = dOption.Value
                                    if fst dictValue && not(Map.containsKey (coordFun coord) st.coordMap)
                                    then
                                        
                                        
                                        let bestWord' = [currentWord']@bestWord
                                        
                                        let amputatedHand = removeSingle id hand
                                        let (f', _) = findAllPossibleWords (coordFun coord) bestWord' currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                        (f', s)
                                    else
                                        let amputatedHand = removeSingle id hand
                                        let (f', _) = findAllPossibleWords (coordFun coord) bestWord currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                        (f', s)
                                else
                                    (f,s)
                            else
                                (f,s)
                        else 
                            let dOption = step ch' dict
                            if dOption.IsSome
                            then
                                let currentWord' = s@[(coord, (id, c))]
                                let dictValue = dOption.Value
                                if fst dictValue && not(Map.containsKey (coordFun coord) st.coordMap)
                                then
                                    
                                    let bestWord' = [currentWord']@bestWord
                                    let amputatedHand = removeSingle id hand
                                    let (f', _) = findAllPossibleWords  (coordFun coord) bestWord' currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                    (f', s)
                                else
                                    let amputatedHand = removeSingle id hand
                                    let (f', _) = findAllPossibleWords (coordFun coord) bestWord currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                    (f', s)
                            else
                                (f,currentAddedTiles)
                        ) (f,s) tile
                ) (bestWord, currentAddedTiles) hand
        else
            //der er optaget på denne plads, lad os steppe hvor vi er
            let (_,(ch,_)) = Map.find coord st.coordMap
            let dOption = step ch dict
            if dOption.IsSome
            then
                if fst dOption.Value && not(Map.containsKey (coordFun coord) st.coordMap)
                then
                    if not (List.isEmpty currentAddedTiles) 
                    then
                        let bestWord' = [currentAddedTiles]@bestWord
                         
                        
                        findAllPossibleWords (coordFun coord) bestWord' currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck
                    else     
                        findAllPossibleWords (coordFun coord) bestWord currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck
                else
                      findAllPossibleWords (coordFun coord) bestWord currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck

            else
                
                (bestWord, list.Empty)    
    
    let findLongestWordInList (words : ((coord * (uint32 * (char * int))) list) list)  =
        List.fold (fun (acc : (coord * (uint32 * (char * int))) list) (newWord: (coord * (uint32 * (char * int))) list) ->
            if newWord.Length > acc.Length then newWord else acc) List.Empty words
                    
    let findMoves (st : State.state) pieces =
        debugPrint("\n\nplayer nr : " + (string)st.playerTurn)        
        if List.isEmpty (st.anchorLists.anchorsForVerticalWords) && List.isEmpty (st.anchorLists.anchorsForHorizontalWords)
        then
            let x = findFirstWord st.hand st.dict pieces (false, List.Empty)
            debugPrint("\n\nfindFirstWord resulted in: " + x.ToString() + "\n")
            snd x
        else            
            
            //horizontal
            let horizontalWords = 
                List.fold (fun acc (anchorPoint,(b,(c,p)))  ->                   
                       fst (findAllPossibleWords  anchorPoint acc List.Empty st st.dict st.hand pieces getNextRightCoord st.crossChecks.checkForHorizontalWords)
                       
                ) List.Empty st.anchorLists.anchorsForHorizontalWords

            if (not (List.isEmpty horizontalWords))
            then
                let word = findLongestWordInList horizontalWords
                debugPrint("\n\n amount of words : " + horizontalWords.Length.ToString())
                debugPrint ("Im gonna play this one horizontally :) " + word.ToString())
                word
            else 
                let verticalWords = 
                    List.fold (fun acc (anchorPoint,(b,(c,p)))  ->
                       
                        fst (findAllPossibleWords anchorPoint acc List.Empty st st.dict st.hand pieces getNextDownCoord st.crossChecks.checkForVerticalWords)
                       
                    ) List.Empty st.anchorLists.anchorsForVerticalWords
                if (not (List.isEmpty verticalWords))
                then
                    let word = findLongestWordInList verticalWords
                    
                    debugPrint("\n\n amount of words : " + verticalWords.Length.ToString())
                    debugPrint ("im gonna play this one vertically :) " + (word).ToString())
                    word
                else 
                    debugPrint "make it clap - i find no word im bad :("
                    List.Empty    
           
    let listToMultiSet h = List.fold (fun acc (x, k) -> add x k acc) empty h

    let rec playerTurnHelper (np : uint32) (next : uint32) (pt : uint32) (f : uint32 Set)  =
        if np.Equals 1u
        then
            1u
        else  
            
            if next.Equals pt
            then
                debugPrint("im forfeit mand")
                failwith "It seems all other players have forfeited."
            else 
                if np >= next && not(Set.contains next f) //the next player is existing and active
                    then next
                else if (np < next) //next player number exceeds the total number of players
                    then playerTurnHelper np (uint32 1) pt f
                else if Set.contains next f //next player has forfeited
                    then playerTurnHelper np (next + uint32 1) pt f
                else
                    failwith "Unexpected error when finding next player."

    let getNextPlayerTurn (st : State.state) = 
        playerTurnHelper st.numOfPlayers (st.playerTurn + uint32 1) st.playerTurn st.forfeited
        
    let updateMap oldmap message= List.fold (fun newmap (coord, brik) -> Map.add coord brik newmap) oldmap message
    
    let chooseWorstPieces hand (amountToRemove: int) (pieces : Map<uint32,tile>)=     
        
        let rec recfold handSoFar (acc : uint32 List)=
            let listhand = MultiSet.toList handSoFar
            match (acc.Length) with
            | bleh when bleh = amountToRemove -> acc
            |_->
                let addToAcc =
                    List.fold (fun (i,p) id ->                    
                    let tile = (Map.find id pieces)
                    let point = snd ((Set.toList tile)[0])
                    if point> p then (id, point) else (i,p)
                    )(uint32 0,0) listhand
                let newHand = (MultiSet.removeSingle (fst addToAcc) handSoFar)
                let newAcc = (fst addToAcc) :: acc
                recfold newHand newAcc
        recfold hand list.Empty
               
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            debugPrint("\n\n Player turn  : " + (string)st.playerTurn + " \n\n")
            if st.playerTurn = st.playerNumber
            
            then
                debugPrint("\n\n Player turn  : " + (string)st.playerTurn + " \n\n")
                let theMoveWellTryToMake : list<coord * (uint32 * (char * int))> = findMoves st pieces
                if not (List.isEmpty theMoveWellTryToMake)
                then 
                    debugPrint((st.playerNumber.ToString() + " IS PLAYING THIS: " + (theMoveWellTryToMake).ToString()))
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) theMoveWellTryToMake) // keep the debug lines. They are useful.
                    send cstream (SMPlay (theMoveWellTryToMake))
                else
                    if st.piecesLeft >= 7
                    then
                        debugPrint("\n\nGonna change 7 pieces")
                        Print.printHand pieces st.hand
                        send cstream (SMChange (MultiSet.toList st.hand))
                    else if st.piecesLeft = 0
                    then
                        
                        debugPrint("\n\nThere are no more tiles to change and i cant find any moves, so thats pretty bad")
                        debugPrint("\n\ngonna try anyway with st.handsixe piece")
                        let tilesToRemove = chooseWorstPieces st.hand (int (MultiSet.size st.hand)) pieces
                        send cstream (SMChange (tilesToRemove))
                    else if st.piecesLeft < 0
                    then
                        //will never print :(
                        debugPrint("\n\nI think that pieces left is negative... so ill try to change pieces and see what the response is")
                        send cstream (SMChange (toList st.hand))
                    else                       
                        if (st.haveJustSwappedTiles )
                        then send cstream (SMPass)
                        else
                            let tilesToRemove = chooseWorstPieces st.hand st.piecesLeft pieces                    
                        
                            debugPrint($"\n\nTrying to swap {tilesToRemove.Length.ToString()} tiles")
                            send cstream (SMChange tilesToRemove)
                    
                
                
            else
                
                ()
                
            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                let playedTiles = List.map (fun x -> (snd x) |> fun y -> ((fst y), uint32 1)) ms
                let handRemoveOld = subtract st.hand (listToMultiSet playedTiles)
                debugPrint("\n\n Old hand without eye is + \n")
                Print.printHand pieces handRemoveOld
                
                debugPrint("\n\n New tiles are  + \n")
                let newPicesAmount = MultiSet.size (listToMultiSet newPieces)
                Print.printHand pieces ((listToMultiSet newPieces))
                
                let handAddNew = sum handRemoveOld (listToMultiSet newPieces)
                debugPrint("\n\n New Hand is   + \n")
                Print.printHand pieces handAddNew
                
                let coordMap' = (updateMap st.coordMap ms)
                let piecesLeft' =
                    if st.piecesLeft - (int)newPicesAmount <= 0
                    then 0
                    else st.piecesLeft - (int) newPicesAmount
                let crossChecks' = updateCrossChecks ms coordMap' st              
                
                let anchorLists' = updateAnchors coordMap'              
                if MultiSet.size handAddNew > (uint)7 then debugPrint("\n\nhand now larger than 7 ")
                let st' =   { st with
                                playerTurn = (getNextPlayerTurn st)
                                points = st.points + points
                                hand = handAddNew
                                coordMap = coordMap'
                                anchorLists = anchorLists'
                                crossChecks = crossChecks'
                                piecesLeft = piecesLeft'
                                haveJustSwappedTiles = false
                }                                        
                
                debugPrint("\n\nYour hand: " + st'.hand.ToString())                
                debugPrint("\nYour points: " + st'.points.ToString() + "\n\n")
                debugPrint("\n ----- ----- ----- ----- ----- \n")
                
                aux st'

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                debugPrint("Other player has played a move" + "\n\n")
                let coordMap' = (updateMap st.coordMap ms)
                let crossChecks' = updateCrossChecks ms coordMap' st              
                
                let anchorLists' = updateAnchors coordMap'
                let piecesLeft' =
                    if st.piecesLeft - ms.Length <= 0
                    then 0
                    else st.piecesLeft - ms.Length
               
                               
                
                let st' =   { st with
                                    playerTurn = (getNextPlayerTurn st)
                                    coordMap = coordMap'
                                    anchorLists = anchorLists'
                                    crossChecks = crossChecks'
                                    piecesLeft = piecesLeft'
                                    haveJustSwappedTiles = false
                }
                aux st'               
                

            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = {st with playerTurn = (getNextPlayerTurn st)}
                 
                aux st'

            | RCM (CMGameOver pointList) ->
                debugPrint("\nRemaining tiles: " + st.piecesLeft.ToString() + "\n")
                for playerResults in pointList
                    do
                        debugPrint("Player " + (fst playerResults).ToString() + ": " + (snd playerResults).ToString() + "points.\n")
            
            | RCM (CMForfeit(pid)) ->
                //A player forfeits
                let st' = {st with forfeited = (Set.add pid st.forfeited)}  
                aux st'
            
            | RCM (CMPassed(pid)) -> 
                //Current player passes
                let currentSt = {st with playerTurn = pid} //this line might be redundant, I put it here just in case...
                let playerNumber' = getNextPlayerTurn st
                let st' = {currentSt with playerTurn = (getNextPlayerTurn st)}
                aux st'
            
            | RCM (CMChangeSuccess(newTiles)) ->
                //You changed your tiles
                let number = MultiSet.size (listToMultiSet newTiles)
                if number = uint 7
                then
                    let st' = {st with
                                hand = listToMultiSet newTiles
                                playerTurn = getNextPlayerTurn st                                 
                                }
                    aux st'
                else                    
                    let tilesToRemove = chooseWorstPieces st.hand (int number ) pieces
                    let tilesToRemoveAsMultiSet = List.fold (fun acc element -> MultiSet.addSingle element acc) MultiSet.empty tilesToRemove
                    let handRemoveOld = subtract st.hand tilesToRemoveAsMultiSet
                    let handAddNew = sum handRemoveOld (listToMultiSet newTiles)         
                    let st' = {st with
                                hand = handAddNew
                                haveJustSwappedTiles = true
                                playerTurn = getNextPlayerTurn st
                                }
                    aux st'
               

            | RCM (CMChange (pid, numTiles)) ->
                //Some other player changed their hand
                
                debugPrint("Player " + pid.ToString() + " changed " + numTiles.ToString() + " tiles.\n\n")
                let st' = 
                    { st with 
                        playerTurn = getNextPlayerTurn st
                    }
                aux st'

            | RCM (CMTimeout pid) ->
                //Some player timed out, which means they pass their turn
                let st' = {st with playerTurn = (getNextPlayerTurn {st with playerTurn = pid})} //again, making sure that we have the right "current" player stored might be redundant.
                aux st'

            //TODO Handle a few of the different Gameplay errors? (see Scrabble.pdf)
            | RGPE err ->
                let tilesLeft =
                    List.fold (fun acc error ->
                        match error with
                        | GPENotEnoughPieces(changeTiles, availableTiles) ->
                            debugPrint($"\n\nCorrecting the amount of pieces left from {st.piecesLeft} to {availableTiles}")
                            (int)availableTiles
                        | _ ->
                            debugPrint("\n\nI am error "+error.ToString())
                            acc
                    ) st.piecesLeft err
                    
                    
                let st' = {st with piecesLeft = tilesLeft}
                aux st'
                //printfn "Gameplay Error:\n%A" err; aux st
        
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
            $"Starting game!
                    number of players = %d{numPlayers}
                    player id = %d{playerNumber}
                    player turn = %d{playerTurn}
                    hand =  %A{hand}
                    timeout = %A{timeout}\n\n"

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                
        let handSet = List.fold (fun acc (x, k) -> add x k acc) empty hand
        
        

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn Set.empty 0 handSet Map.empty (State.mkAnchors List.empty List.empty) (State.mkCrossChekcs Map.empty Map.empty)) 
    
    
    
     
            

    
    
