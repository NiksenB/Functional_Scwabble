module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)];;
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 + v2)))    
    let sub a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 - v2)))    
    let mul a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 * v2))) 
    let div a b = 
        a >>= (fun v1 -> b >>= (fun v2 -> 
                if v2 <> 0
                then ret (v1 / v2)
                else fail (DivisionByZero) 
            )
        )
    let _mod a b = 
        a >>= (fun v1 -> b >>= (fun v2 -> 
                if v2 <> 0
                then ret (v1 % v2)
                else fail (DivisionByZero) 
            )
        )
        
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N (b) -> ret b
        | V (b) -> lookup b
        | WL -> wordLength
        | PV (b) -> arithEval b >>= (fun v -> pointValue v)
        | Add (x,y) -> add (arithEval x) (arithEval y)
        | Sub (x,y) -> sub (arithEval x) (arithEval y)
        | Mul (x,y) -> mul (arithEval x) (arithEval y)
        | Div (x,y) -> div (arithEval x) (arithEval y)
        | Mod (x,y) -> _mod (arithEval x) (arithEval y)
        | CharToInt (b) -> charEval b >>= (fun v -> ret (System.Convert.ToInt32 v))
    and charEval c : SM<char> = 
        match c with
        | C (d) -> ret d 
        | CV (d) -> (arithEval d) >>= (fun v -> (characterValue v))
        | ToUpper (d) -> match d with 
                         | C (e) -> ret (System.Char.ToUpper e)
                         | _ -> charEval d >>= (fun v -> ret (System.Char.ToUpper v))
        | ToLower (d) -> match d with 
                         | C (e) -> ret (System.Char.ToLower e)
                         | _ -> charEval d >>= (fun v -> ret (System.Char.ToLower v))
        | IntToChar (d) -> (arithEval d) >>= (fun v -> ret ((System.Char.ConvertFromUtf32 v).[0])) 

    let rec boolEval b : SM<bool> = 
       match b with
       | TT -> ret true
       | FF -> ret false
       | AEq (c,d) -> arithEval c >>= (fun v -> arithEval d >>= (fun w -> ret (v = w)))
       | ALt (c,d) -> arithEval c >>= (fun v -> arithEval d >>= (fun w -> ret (v < w)))
       | Not c -> (boolEval c) >>= (fun v -> ret (not v))
       | Conj (c,d) -> boolEval c >>= (fun v1 -> boolEval d >>= (fun v2 -> ret (v1 && v2))) 
       | IsVowel c -> (charEval c) >>= (fun v -> ret (match System.Char.ToLower v with
                                                        | 'a' | 'e' | 'i' | 'o' |'u' |'y' -> true
                                                        | _ -> false))

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

   
    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        |Skip -> ret ()
        |Ass (x,a) -> 
            arithEval a >>= (update x)   
        |Seq (stm1, stm2) -> stmntEval stm1 >>>=  stmntEval stm2 
        |ITE (guard, stm1, stm2) ->
            (boolEval guard)  >>= (fun x  -> push >>>= if x then pop >>>= (stmntEval stm1) else pop >>>= stmntEval stm2)            
        |While (guard, stm) ->
            (boolEval guard)  >>= (fun b  -> push >>>= if b then pop >>>= stmntEval stm >>>= stmntEval (While (guard, stm))  else  pop >>>= ret ())
        |Declare x -> declare x 


(* Part 3 (Optional) *)
    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a =
        prog{
        match a with
            | N n -> return n
            | V v -> return!  lookup v
            | PV x ->  
                        let! p = arithEval x
                        return! (pointValue p)
            | WL ->  return! wordLength
            | Add (a, b) -> return!  add (arithEval a) (arithEval b) 
            | Sub (a, b) -> return!  sub (arithEval a) (arithEval b)  
            | Mul (a, b) -> return!  mul (arithEval a) (arithEval b) 
            | Div (a, b) -> return! (div (arithEval a) (arithEval b))
            | Mod (a,b ) -> 
                        let! x = arithEval a
                        let! y = arithEval b
                        if y <> 0 then return (x % y) else return! fail DivisionByZero                
            | CharToInt ch ->
                    let! x = charEval ch
                    return System.Char.ToUpper x |> (int)
            }
    let charEval2 c =
        prog{
            match c with
            |C ch -> return ch
            |ToUpper ch ->
                let! x' = charEval ch
                return (System.Char.ToUpper x')
            |ToLower ch ->
                let! x' = charEval ch
                return (System.Char.ToLower x') 
            |CV i ->
                let! x = arithEval i
                return! characterValue x
            |IntToChar i' ->
                let! i'' = arithEval i'
                return char(i'')
        }
    let rec boolEval2 b =
        prog{
            match b with 
            | TT           -> return true                   
            | FF           -> return false        
            | AEq (a,b)     ->
                let! x = arithEval a
                let! y = arithEval  b
                return (x = y)
            | ALt (a,b)     ->
                let! x = arithEval a
                let! y = arithEval  b
                return (x > y)
            | Not b         ->
                let! b' = boolEval b
                return (not b')            
            | Conj (a,b)    ->
                let! x = boolEval a
                let! y = boolEval  b
                return  (x && y)
        }

    let stmntEval2 stm =
        prog{
            match stm with
            |Skip -> return ()
            |Ass (x,a) -> 
                let! a' = arithEval a
                return! (update x a')   
            |Seq (stm1, stm2) ->
                do! stmntEval stm1
                do! stmntEval stm2 
            |ITE (guard, stm1, stm2) ->
                let! g = boolEval guard
                do! push
                if g
                then
                    do! pop
                    return! (stmntEval stm1)
                else
                    do! pop
                    return! (stmntEval stm2)            
            |While (guard, stm) ->
                let! g = boolEval guard
                do! push
                if g
                then
                    do! pop
                    do! stmntEval stm
                    return! stmntEval (While (guard, stm))
                else
                    do! pop
                    return ()
            |Declare x -> return! (declare x )
        }

(* Part 4 (Optional) *)
    
    

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    
    type coord = int * int
    
    type boardFun = coord -> Result<squareFun option, Error>
    
    //LOOK HERE, WE NEED THIS. IT IS FROM 6.12 BUT BASED ON WORK WE DID IN 3.7
    
    
    //let stmntToSquareFun (stm : stm) : squareFun = fun mkState
   
    
    let stmntToBoardFun (stm : stm) (m: Map<int, squareFun> ) : boardFun =
        let sf c =
            let vars = [("_x_", fst(c)); ("_y_",snd(c)); ("_result_",0)]
            let set = [("_x_"); ("_y_"); ("_result_")]
            let i = stmntEval2 stm >>>= lookup "_result_" |> evalSM ( mkState vars [] set )
            match i with
            | Success (a) ->  Success(m.TryFind a)
            | Failure (a) -> Failure ( a)
            
        sf  


    //SHOULD THE TYPE DEFINITIONS AND FUNCTIONS BELOW BE HERE IN EVAL, OR IN PARSER.fs? SOME OF THE LINES ARE CURRENTLY IN BOTH PLACES

    let stmntToSquareFun stm : squareFun= 
        let sf word pos acc =
                let vars = [("_pos_", pos); ("_acc_",acc); ("_result_",0)]
                let set = [("_pos_"); ("_acc_"); ("_result_")]
                stmntEval2 stm >>>= lookup "_result_" |> evalSM ( mkState vars  word set )           
             
        sf 

    

    

    

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }


    

    // let stmntToBoardFun stm m = failwith "Not implemented" //stmnt -> Map<int, squareFun> -> boardFun

    // let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    
