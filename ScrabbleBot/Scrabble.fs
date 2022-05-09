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
    }
    
    
    
    type coordTile = coord * (uint32 * (char *int))

    let mkCrossChekcs h v = {checkForHorizontalWords = h; checkForVerticalWords = v;}
    
    let mkAnchors h v = {anchorsForHorizontalWords = h; anchorsForVerticalWords =v; }
    let mkState b d np pn pt f p h cm al cc = 
        {board = b; dict = d;  numOfPlayers = np; playerNumber = pn; playerTurn = pt; forfeited = f; points = p; hand = h; coordMap = cm; anchorLists = al; crossChecks = cc }

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
    let crossChecks st    = st.crossChecks
    


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
                |true,true -> ((key,value) :: anchorListHorizontal, (key,value) :: anchorListVertical)
                |true,false ->  ((key,value) :: anchorListHorizontal, anchorListVertical)
                |false, true -> (anchorListHorizontal, (key,value) :: anchorListVertical)
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
        forcePrint ("\n\ncrossCheckGenericTwoBefore
         is called for brik: " + brik.ToString())
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
                //forcePrint("after left and left this should be A : " + wordBelow.ToString())
                let validLetters = lookUpWithStringStartingAtEveryLetterOfAlphabet wordBelow state.dict                    
                Map.add emptyTile validLetters crossCheckRules
            | (false, _) ->
                //I have a neighbor and possibly more.
                let notABrik = Map.find (nextNeighBorCoord coord) coordMap
                let brik' = ((nextNeighBorCoord coord), notABrik)
                crossCheckGenericTwoBefore crossCheckRules brik' coordMap state checkNeighborIsFree nextNeighBorCoord goToStartOfWordBefore goToStartOfWordAfter
            | (_,_) -> crossCheckRules
            
    
    let crossCheckAfter (crossCheckRules : Map<coord, Set<char>>) brik (coordMap : Map<coord, uint32 * (char * int)>) (state : State.state) isNeighborFree nextNeighborCoord goToStartOfWordBefore=
        //the crosscheckDownDown only needs to find if the two below are free, as the crosscheckupandup will have found any empty tile between to vertical words.
        let coord = fst brik
    
        //forcePrint ("\n\ncrossCheckAfter is called for brik: " + brik.ToString())

        match (isNeighborFree coord coordMap, isNeighborFree (nextNeighborCoord coord) coordMap) with 
            | true, true ->
              //forcePrint("\n\nisNeighborFree returns true on coord: " + (nextNeighborCoord coord).ToString() + " and true on coord :" + (nextNeighborCoord(nextNeighborCoord coord)).ToString() )
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
            | _ -> crossCheckRules
            
    let crossCheckUpAndUp crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckGenericTwoBefore
         crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state hasNotUpNeighbor getNextUpCoord goToStartOfWordAbove goToStartOfWordBelow
    
    let crossCheckDownAndDown crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckAfter crossCheckRules brik  coordMap state hasNotDownNeighbor getNextDownCoord goToStartOfWordAbove        
    let crossCheckLeftAndLeft crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckGenericTwoBefore
         crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state hasNotLeftNeighbor getNexLeftCoord goToStartOfWordLeft goToStartOfWordRight
    
    let crossCheckRightAndRight crossCheckRules (brik : coord * (uint32 * (char * int))) coordMap state =
        crossCheckAfter crossCheckRules brik  coordMap state hasNotRightNeighbor getNextRightCoord goToStartOfWordLeft 
           
    let rec updateCrossChecks (newMoves : (coord * (uint32 * (char * int))) list) coordMap (st : State.state)=
        
        
        let rec runThroughNewTiles (upAndDown, leftAndRight) moves =
            match moves with
            | [] -> (upAndDown, leftAndRight)
            | brik :: brikker ->
                //forcePrint "\n\nJeg kigger oven over mig nu!"
                let upAndDown' = crossCheckUpAndUp upAndDown brik coordMap  st
                //forcePrint ("\nupAndDown' for letter " + brik.ToString() + " is: " + upAndDown'.ToString())
                // this finds the one where there is two down free
                //forcePrint "\n\nJeg kigger neden under mig nu!"
                let upAndDownResult = crossCheckDownAndDown upAndDown' brik coordMap  st
                //forcePrint ("\nupAndDownResult for letter " + brik.ToString() + " is: " + upAndDownResult.ToString())
                
                //forcePrint "\n\nJeg kigger til venstre for mig nu!"
                let leftAndRight' = crossCheckLeftAndLeft leftAndRight brik coordMap st
                //forcePrint ("\nleftAndRight' for letter " + brik.ToString() + " is: " + leftAndRight'.ToString())
                
                //forcePrint "\n\nJeg kigger til højre for mig nu!"
                let leftAndRightResult = crossCheckRightAndRight leftAndRight' brik coordMap st
                //forcePrint ("\nleftAndRightResult' for letter " + brik.ToString() + " is: " + leftAndRightResult.ToString())
                
                //forcePrint ("\n\nWe now call runThroughNewTiles again using upAndDownResult and leftAndRightResult")
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
                                findFirstWord amputatedHand dict' pieces acc'
                        else //we're headed down a branch with no destination, skip this branch/combination
                            result

                ) acc tile
            ) result hand
    
    //let udenOmFold coord hand crossCheck pieces=
    //    if Map.containsKey coord crossCheck
    //    then
    //        fold (fun acc id' amountOfElements->
    //            let tile = Map.find id' pieces
    //            let newset = Set.fold (fun accWithChar c ->
    //                    let ch' = fst c
    //                    if Set.contains ch' (Map.find coord crossCheck)
    //                    then                            
    //                        MultiSet.add id' accWithChar
    //                    else
    //                        accWithChar
    //             
    //                ) acc tile
    //            let newAcc = removeSingle id' acc
    //            MultiSet.add newAcc 
    //            ) MultiSet.empty hand
    //    else hand
    
    // let rec findWordOldAndUgly (coord, (id , (ch , point))) currentWord (st : State.state) (dict : Dict) (haveAddedOwnLetter : bool) (hand : MultiSet<uint32>) (pieces : Map<uint32, tile>) coordFun crossCheck =
    //     let isOccupiedNextToMe = Map.containsKey (coordFun coord) st.coordMap
        
    //     if not isOccupiedNextToMe
    //     then  
    //         let nextDict = step ch dict
    //         if Option.isSome nextDict
    //         then
    //             let isValidWord = fst nextDict.Value
    //             if isValidWord && haveAddedOwnLetter
    //             then
    //                 (true,snd currentWord)
    //             else                    
    //                 fold (fun acc id' amountOfElements->
    //                     if fst acc
    //                     then acc
    //                     else
    //                         let tile = Map.find id' pieces
                            

    //                         Set.fold (fun accWithChar c -> //for each possible char value a tile can have, try build word
    //                             let ch' = fst c
    //                             let point' = snd c
    //                             let coord' = coordFun coord
    //                             if Map.containsKey coord' crossCheck
    //                             then
    //                                 forcePrint("print good when coord is (1,1) " + coord'.ToString())
    //                                 forcePrint("crosscheckMap for right coord after E : "  + (Map.find coord' crossCheck).ToString() )

    //                                 if Set.contains ch' (Map.find coord crossCheck)
    //                                 then
    //                                     let dict' = snd nextDict.Value
    //                                     let currentWord'  = (false, snd currentWord@[(coord', (id' , (ch' , point')))] )
    //                                     let newMultiSet = removeSingle id' hand 
                                
    //                                     findWordOldAndUgly (coord', (id' , (ch' , point'))) currentWord' st dict' true newMultiSet pieces coordFun crossCheck
    //                                 else
    //                                     acc

    //                             else
    //                                 let dict' = snd nextDict.Value
    //                                 let currentWord'  = (false, snd currentWord@[(coord', (id' , (ch' , point')))] )
    //                                 let newMultiSet = removeSingle id' hand 
                                
    //                                 findWordOldAndUgly (coord', (id' , (ch' , point'))) currentWord' st dict' true newMultiSet pieces coordFun crossCheck
    //                         ) acc tile  

    //                 ) currentWord hand
                    
    //         else
    //             (false, snd currentWord)
    //     else
    //         let letter = Map.find (coordFun coord) st.coordMap
    //         let id' = (fst letter)
    //         let dict' = snd (step ch dict).Value
    //         let tile = Map.find id' pieces
    //         let point' = snd (snd letter)
    //         let coord' = coordFun coord
    //         let ch' = (fst (snd letter))
            
    //         findWordOldAndUgly (coord', (id', (ch', point'))) currentWord st dict' haveAddedOwnLetter hand pieces coordFun crossCheck

    let rec findWord coord (finishedWords : ((coord * (uint32 * (char * int))) list) list ) (currentAddedTiles : ((coord * (uint32 * (char * int))) list)) (st : State.state) (dict : Dict) (hand : MultiSet<uint32>) (pieces : Map<uint32, tile>) coordFun crossCheck =
       
        let isCoordOccupied = Map.containsKey coord st.coordMap
        
        if not isCoordOccupied
        then
            if isEmpty hand
            then
                (finishedWords, list.Empty)
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
                                    if fst dictValue
                                    then
                                        forcePrint("i have found this word ")
                                        let finishedWords' = finishedWords@[currentWord']
                                        let amputatedHand = removeSingle id hand
                                        let (f', _) = findWord (coordFun coord) finishedWords' currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                        (f', s)
                                    else
                                        let amputatedHand = removeSingle id hand
                                        let (f', _) = findWord (coordFun coord) finishedWords currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
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
                                if fst dictValue
                                then
                                    let finishedWords' = finishedWords@[currentWord']
                                    let amputatedHand = removeSingle id hand
                                    let (f', _) = findWord (coordFun coord) finishedWords' currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                    (f', s)
                                else
                                    let amputatedHand = removeSingle id hand
                                    let (f', _) = findWord (coordFun coord) finishedWords currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                    (f', s)
                            else
                                (f,currentAddedTiles)
                        ) (f,s) tile
                ) (finishedWords, currentAddedTiles) hand
        else
            //der er optaget på denne plads, lad os steppe hvor vi er
            let (_,(ch,_)) = Map.find coord st.coordMap 
            let dOption = step ch dict
            if dOption.IsSome
            then
                if fst dOption.Value
                then
                    if not (List.isEmpty currentAddedTiles)
                    then
                        let finishedWords' = finishedWords@[currentAddedTiles]
                        findWord (coordFun coord) finishedWords' currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck
                    else     
                        findWord (coordFun coord) finishedWords currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck
                else
                      findWord (coordFun coord) finishedWords currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck

            else
                forcePrint ("\nI hit a dead end on " + currentAddedTiles.ToString() + "\n")
                (finishedWords, list.Empty)

    let rec findWordBestHeuristic coord (bestWord : ((coord * (uint32 * (char * int))) list) ) (currentAddedTiles : ((coord * (uint32 * (char * int))) list)) (st : State.state) (dict : Dict) (hand : MultiSet<uint32>) (pieces : Map<uint32, tile>) coordFun crossCheck =
       
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
                                    if fst dictValue
                                    then
                                        forcePrint("i have found this word " + currentWord'.ToString())
                                        let bestWord' =
                                            if (bestWord.Length > currentWord'.Length)
                                            then bestWord
                                            else currentWord'
                                        let amputatedHand = removeSingle id hand
                                        let (f', _) = findWordBestHeuristic (coordFun coord) bestWord' currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                        (f', s)
                                    else
                                        let amputatedHand = removeSingle id hand
                                        let (f', _) = findWordBestHeuristic (coordFun coord) bestWord currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
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
                                if fst dictValue
                                then
                                    let bestWord' =
                                            if (bestWord.Length > currentWord'.Length)
                                            then bestWord
                                            else currentWord'
                                    let amputatedHand = removeSingle id hand
                                    let (f', _) = findWordBestHeuristic  (coordFun coord) bestWord' currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
                                    (f', s)
                                else
                                    let amputatedHand = removeSingle id hand
                                    let (f', _) = findWordBestHeuristic (coordFun coord) bestWord currentWord' st (snd dictValue) amputatedHand pieces coordFun crossCheck
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
                if fst dOption.Value
                then
                    if not (List.isEmpty currentAddedTiles)
                    then
                        let bestWord' =
                            if (bestWord.Length > currentAddedTiles.Length)
                            then bestWord
                            else currentAddedTiles
                        findWordBestHeuristic (coordFun coord) bestWord' currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck
                    else     
                        findWordBestHeuristic (coordFun coord) bestWord currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck
                else
                      findWordBestHeuristic (coordFun coord) bestWord currentAddedTiles st (snd dOption.Value) hand pieces coordFun crossCheck

            else
                forcePrint ("\nI hit a dead end on " + currentAddedTiles.ToString() + "\n")
                (bestWord, list.Empty)    
    
    
    let findOneMove (st : State.state) pieces =
        if List.isEmpty (st.anchorLists.anchorsForVerticalWords) && List.isEmpty (st.anchorLists.anchorsForHorizontalWords)
        then 
            let x = findFirstWord st.hand st.dict pieces (false, List.Empty)
            forcePrint("findFirstWord resulted in: " + x.ToString() + "\n")
            snd x
        else 
            //horizontal
            let horizontalWords = 
                List.fold (fun acc (anchorPoint,(b,(c,p)))  ->
                   if List.isEmpty acc
                   then
                       let dict' = step c st.dict
                       //TODO DONT JUST ASSUME THAT THIS WORKS WITH THE DICT
                       fst (findWordBestHeuristic (getNextRightCoord anchorPoint) acc List.Empty st (snd dict'.Value) st.hand pieces getNextRightCoord st.crossChecks.checkForHorizontalWords)
                   else
                       acc
                ) List.Empty st.anchorLists.anchorsForHorizontalWords

            if (not (List.isEmpty horizontalWords))
            then
                forcePrint ("Im gonna play this one horizontally :) " + (horizontalWords).ToString())
                horizontalWords
            else 
                let verticalWords = 
                    List.fold (fun acc (anchorPoint,(b,(c,p)))  -> 
                        if List.isEmpty acc 
                        then
                            let dict' = step c st.dict
                            fst (findWordBestHeuristic (getNextDownCoord anchorPoint) acc List.Empty st (snd dict'.Value) st.hand pieces getNextDownCoord st.crossChecks.checkForVerticalWords)
                        else
                            acc
                    ) List.Empty st.anchorLists.anchorsForVerticalWords
                if (not (List.isEmpty verticalWords))
                then
                    forcePrint ("im gonna play this one vertically :) " + (verticalWords).ToString())
                    verticalWords
                else 
                    //TODO : change briks o
                    forcePrint "make it clap - i find no word im bad :("
                    verticalWords

            
        
           
    let listToMultiSet h = List.fold (fun acc (x, k) -> add x k acc) empty h

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
            
            //forcePrint("results: "+words.ToString()+"\n\n")
            //List.fold (fun acc x -> forcePrint("Det her er et valid ord: "+x.ToString()+"\n\n")) () words

            //TODO should we somehow check that st.playerTurn = st.playerNumber before trying to play?
            
            Print.printHand pieces (State.hand st)
            let theMoveWellTryToMake : list<coord * (uint32 * (char * int))> = findOneMove st pieces
            let x = List.fold (fun a b -> forcePrint("TILE : "+b.ToString())) () theMoveWellTryToMake
            if not (List.isEmpty theMoveWellTryToMake)
            then 
                forcePrint((st.playerNumber.ToString() + " IS PLAYING THIS: " + (theMoveWellTryToMake).ToString()))
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) theMoveWellTryToMake) // keep the debug lines. They are useful.
                send cstream (SMPlay (theMoveWellTryToMake))
            else 
                send cstream (SMChange (MultiSet.toList st.hand)) //TODO : (SMChange list-of-uint)
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) theMoveWellTryToMake) // keep the debug lines. They are useful.



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
                let handRemoveOld = subtract st.hand (listToMultiSet playedTiles)
                let handAddNew = sum handRemoveOld (listToMultiSet newPieces)
                let coordMap' = (updateMap st.coordMap ms) 
                //forcePrint "\n\ncoordmap done"
                let crossChecks' = updateCrossChecks ms coordMap' st
                
                //forcePrint "\n\ncrosschecks vertical words"
                //Map.fold (fun _ key value -> forcePrint(key.ToString() + value.ToString())) () crossChecks'.checkForVerticalWords
                
               // forcePrint "\n\ncrosschecks horizontal words"
                //Map.fold (fun _ key value -> forcePrint(key.ToString() + value.ToString())) () crossChecks'.checkForHorizontalWords

                let anchorLists' = updateAnchors coordMap'
                
                
                //forcePrint("anchor done" + anchorLists'.ToString() + "\n\n")

                //orcePrint "\n\nanchorlist vertical"
                //List.fold (fun _ y -> forcePrint(y.ToString())) () anchorLists'.anchorsForVerticalWords

                //forcePrint "\n\nanchorlist horizontal"
                //List.fold (fun _ y -> forcePrint(y.ToString())) () anchorLists'.anchorsForHorizontalWords

                let st' =   { st with
                                playerTurn = (getNextPlayerTurn st)
                                points = st.points + points
                                hand = handAddNew
                                coordMap = coordMap'
                                anchorLists = anchorLists'
                                crossChecks = crossChecks'
                }                                        
                
                forcePrint("\n\nYour hand: " + st'.hand.ToString())
                //forcePrint("The board: " + st'.coordMap.ToString())
                //forcePrint("Your player number: " + st'.playerNumber.ToString() + "\n\n")
                //forcePrint("Your state: " + st'.ToString() + "\n\n")
                //forcePrint("Next player: " + st'.playerTurn.ToString() + "\n\n")
                forcePrint("\nYour points: " + st'.points.ToString() + "\n\n")
                //forcePrint("Crosscreck: " + st'.crossCheck.ToString() + "\n\n")
                forcePrint(" ----- ----- ----- ----- -----")
                
                aux st'

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                forcePrint("HOT DIGGIDY DAWG" + "\n\n")
                let coordMap' = (updateMap st.coordMap ms)        
                forcePrint("HOT DIGGIDY DAWG" + "\n\n")
                let st' = {st with
                            playerTurn = (getNextPlayerTurn st);
                            coordMap = coordMap'
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
                        hand = listToMultiSet newTiles;
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
    
    
    
     
            

    
    
