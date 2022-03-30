module internal MultiSet
    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32> 

    let empty = M (Map.empty)
    let isEmpty (M (s)) = Map.isEmpty s
    let size (M (s)) = Map.fold (fun acc _ v -> acc + (int) v) 0 s |> uint32
    let contains a (M (s)) = s.ContainsKey a
    let numItems a (M (s)) = if s.ContainsKey a then Map.find a s else uint32 0
    let add a n (M (s)) = if s.ContainsKey a then M (Map.add a ((s.Item a) + n) s) else M (s.Add (a, n))
    let addSingle a (M (s)) = if s.ContainsKey a then M (Map.add a ((s.Item a) + uint32 1) s) else M (s.Add (a, uint32 1))
    let remove a n (M (s)) = if (s.TryFind a).IsSome && s.Item a > n then M (Map.add a ((s.Item a) - n) s) else M (s.Remove a)
    let removeSingle a (M (s)) = if (s.TryFind a).IsSome && s.Item a > uint32 1 then M (Map.add a ((s.Item a) - uint32 1) s) else M (s.Remove a)
    let fold f acc (M (s)) = Map.fold f acc s
    let foldBack f (M (s)) acc = Map.foldBack f s acc
    let ofList lst = M (List.fold (fun s a -> 
            if s.ContainsKey a 
            then Map.add a ((Map.find a s) + uint32 1) s 
            else Map.add a (uint32 1) s)
        Map.empty lst)
    let toList (M (s)) = Map.fold (fun l a occurences -> [ for _ in 1 .. int (occurences) -> a ] |> List.append l) List.Empty s
    let map f m = ofList (List.map f (toList m))
    let union (M (s)) (M (t)) = 
        M ( Map.fold (fun uFromT a v -> 
                if Map.containsKey a t && t.Item a > v
                then uFromT
                else Map.add a v uFromT) 
        t s)
    let sum (M (s)) (M (t)) = 
        M ( Map.fold (fun uFromT a v -> 
                if Map.containsKey a t
                then Map.add a (t.Item a + v) uFromT
                else Map.add a v uFromT) 
        t s)
    let subtract s1 s2 =
        fold (fun acc elem _ -> 
            remove elem (numItems elem s2) acc) s1 s2
    let intersection (M (s)) (M (t)) =
        M ( Map.fold (fun u a v -> 
                if Map.containsKey a t
                then (
                    if t.Item a > v 
                    then Map.add a v u 
                    else Map.add a (t.Item a) u) 
                else u) 
        Map.empty s) 