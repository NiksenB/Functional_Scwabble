module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = s.vars.Tail}))

    let wordLength : SM<int> = 
        S (fun s -> Success (s.word.Length, s))

    let characterValue (pos : int) : SM<char> = 
        S (fun s -> 
            match List.tryItem pos s.word with
            | Some so -> Success(fst so, s)
            | None -> Failure(IndexOutOfBounds pos) //fail (IndexOutOfBounds (pos))
        )   

    let pointValue (pos : int) : SM<int> = 
        S (fun s -> 
            match List.tryItem pos s.word with
            | Some so -> Success(snd so, s)
            | None -> Failure(IndexOutOfBounds pos) //fail (IndexOutOfBounds (pos))
        )   

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    //TODO LOOK HERE: JUST ADDED BUT MAI CAN REPLACE WITH BETTER VERSION? 
    let declare (var : string) : SM<unit> =
        S (fun s -> 
            if s.reserved.Contains(var)
            then 
                Failure (ReservedName var)
            // elif List. Map.s.vars ....
            // then
            //     Failure (VarExists var)
            else 
                Success ((), {s with vars = (List.append s.vars [Map.add var 0 Map.empty])})
            )
    
    let declareMai (var : string) : SM<unit> =
        S (fun s ->
            if s.reserved.Contains var then Failure(ReservedName var)
            else if s.vars.Head.ContainsKey var then Failure(VarExists var)
            else
                let newM = Map.empty
                let m = newM.Add(var, 0)
                Success ((), {s with vars = m :: s.vars})
            )
            
    let update (var : string) (value : int) : SM<unit> = 
        let rec aux n =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind var m with
                | Some _ -> Some (n, Map.add var value m)
                | None   -> aux (n-1) ms
        S (fun s -> 
                match s, aux s.vars.Length (s.vars) with
                | s, Some (i,q) -> Success ((), {s with vars = (s.vars.GetSlice (Some(0),Some(i))) |> List.append [q] })
                | _, None   -> Failure (VarNotFound var))