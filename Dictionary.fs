module Dictionary
open System.Collections.Generic

type Dict = 
    | Leaf of bool
    | Node of bool * Dictionary<char, Dict>

type cDDict = Dictionary<char, Dict>

let empty () = Leaf (false)

let rec insert (s: string) =
    function
    //Dict is longer than String
    | Leaf _ when s.Length = 0 -> Leaf true
    | Node (_, D) when s.Length = 0 -> Node(true, D)
    
    //String is longer than Dict
    | Leaf b -> 
        let replacement = cDDict()
        //replacement.[s.[0]] <- insert s.[1..] (empty ()) //mapName.[key] <- value
        replacement.Add (s.[0], insert s.[1..] (empty ()))
        Node(b, replacement)
    
    //They are both longer, insert new node or replace current node
    | Node (b, D) ->
        let c = s.[0]
        match D.TryGetValue c with
        | (true, value) -> D.[c] <- insert s.[1..] value
        | (false, _) -> D.[c] <- insert s.[1..] (empty ())
        Node(b, D)

let rec lookup (s : string) =
    function
    | Leaf b when s.Length = 0 -> b
    | Leaf _ -> false

    | Node (b, _) when s.Length = 0 -> b

    | Node (_, D) ->
        match D.TryGetValue s.[0] with
        | (true, v) -> lookup s.[1..] v
        | (false, _) -> false

let step (c : char) (D : Dict) =
    match D with
    // | Node(_, _) ->  
    //     if lookup (string c) D
    //     then Some(true, D)
    //     else Some(false, D)
    | Node (b, _) -> Some(b, D)
    | Leaf(_) -> None