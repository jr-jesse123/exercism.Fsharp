module Dominoes

(*
    specs:
        create a chain with all provided domino pieces (represented by int tuples). in any possible order
        the snd tuple element should always match the fst tuple element of the next item
        we can have repeated tuples
*)
(*
    stpes:
    1 - grab first element of provided list to a separate list
    2 - 
*)

let rec permutations list =
    let rec insertAllPositions x lst =
        match lst with
        | [] -> [[x]] 
        | head :: tail -> 
            let bottom = (insertAllPositions x tail)
            (x :: lst) :: (List.map (fun l -> head :: l) bottom)
          
    match list with
    | [] -> seq {[]} 
    | head :: tail ->
        permutations tail
        |> Seq.collect (insertAllPositions head)
 

let rec withInvertedStones (lst:('a * 'a) list) =
    match lst with
    | [] -> [[]]
    | [(a,b)] -> [[(a,b)]; [(b,a)]]
    | (head):: tail ->
        let (a,b) = head
        let bottom = withInvertedStones tail
        List.map (fun l -> head::l) bottom
        @ List.map (fun l -> (b,a)::l) bottom
 

let rec areAllConnected lst fstElement=
    match lst with
    |[] -> true
    |[x] -> snd x = fst fstElement
    |(a,b)::xs ->
        if b = fst xs.Head then
            areAllConnected xs fstElement
        else
            false

let rec canChain input =
    if List.isEmpty input then
        true
    else
        let allPermutations = 
            permutations input
            |> Seq.collect withInvertedStones
            
        allPermutations 
        |> Seq.exists (fun permutation -> areAllConnected permutation permutation.Head)
    
   