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
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

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
       | IsLetter c -> (charEval c) >>= (fun v -> ret (System.Char.IsLetter v))
       | IsDigit c -> (charEval c) >>= (fun v -> ret (System.Char.IsDigit v))

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    