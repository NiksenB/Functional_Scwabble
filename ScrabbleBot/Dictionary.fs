module Dictionary

    open System.Collections.Generic
    open Microsoft.FSharp.Collections
    
    type Dict =
        | Leaf of bool
        | Node of bool * Dictionary<char, Dict>
    
    
    let empty () = Leaf false     
        
    let rec insert (s : string) dict =
        match dict with
        | Leaf _ when s = "" -> Leaf true 
        | Node (_, m) when s = "" -> Node(true, m)
        | Leaf b -> 
            let firstChar = s.Chars(0)
            let newDict = Dictionary<char, Dict> ()
            newDict.Add(firstChar, (insert (s.Remove(0,1)) (empty ())))
            Node(b, newDict)
        | Node (b, m) ->
            let firstChar = s.Chars(0)
            if (fst (m.TryGetValue firstChar))
            then
                m.[firstChar] <- insert (s.Remove(0,1)) m.[firstChar]
                Node(b, m)
            else
                m.Add(firstChar, insert (s.Remove(0,1)) (empty ()))
                Node(b, m)
                
    let rec lookup (s : string) dict =
        match dict with
        | Leaf b when s = "" -> b
        | Leaf _ -> false
        | Node(b, m) when s = "" -> b
        | Node(b, m) ->
            let firstChar = s.Chars(0)
            if (fst (m.TryGetValue firstChar))
            then
                lookup (s.Remove(0,1)) m.[firstChar]
            else
                false
    
    let step c dict =
        match dict with
        | Leaf _ -> None 
        | Node(b, m) ->
            if m.ContainsKey c
            then
                match m.[c] with
                | Leaf true -> Some (true, m.[c])
                | Node(b, _) when b = true -> Some (true, m.[c])
                | _ -> Some(false, m.[c])
            else None