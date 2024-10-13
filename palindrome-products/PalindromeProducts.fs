module PalindromeProducts

open System

[<TailCall>]
let products (range:'a list ) acc= 
    let rec loop range acc =
        match range with
        | [x] ->     [x * x , (x,x)] @ acc 
        | head::tail ->
            //let base' = products tail
            let current = 
                [   yield  head * head, (head, head)
                    for x in tail do
                        let value = head * x
                        
                        yield value, (head,x)]
            let newAcc =   current @ acc 
            loop tail newAcc
    
    loop range []
    |> List.rev
    //|> List.sortBy (fun (a,(b,c)) -> b)
//TODO: ELIMINAR ESTE SORT CARO, DEPOIS  CORRIGIR O ISPALINDROME ABAIXO  
let isPalindrome' nr =
    let rec loop nr reversed = 
        if nr = 0 then reversed
        else loop (nr / 10) ((nr % 10) * 10 + reversed)
    let reversed = loop nr 0
    nr = reversed

//TODO: CHECK WITH MUTABLE STATE
let isPalindrome (a,_) =
    //isPalindrome' a
    a.ToString().ToCharArray() |> Array.rev |> String |> int |> (=) a

module List = 
    let tryMaxBy (mapper:'a -> 'b )(lst: 'a list) : 'a option = 
        match lst with
        |[] -> None
        | lst  -> lst |> List.maxBy mapper |> Some

    let tryMinBy (mapper:'a -> 'b )(lst: 'a list) : 'a option = 
        match lst with
        |[] -> None
        | lst  -> lst |> List.minBy mapper |> Some


let minFactor = 1
let maxFactor = 9
let largest minFactor maxFactor = 
    products [minFactor..maxFactor] []
    |> List.filter isPalindrome
    |> List.sortByDescending fst
    //|> List.filter (fun x -> List.maxBy fst (products [minFactor..maxFactor]) = x )
    |> List.groupBy fst
    |> List.map (fun (label, result) -> Some label,List.map snd result)
    |> List.tryHead
    |> Option.defaultValue (None, [])

let smallest minFactor maxFactor = 
    products [minFactor..maxFactor] []
    |> List.filter isPalindrome
    |> List.sortBy fst
    //|> List.filter (fun x -> List.maxBy fst (products [minFactor..maxFactor]) = x )
    |> List.groupBy fst
    |> List.map (fun (label, result) -> Some label,List.map snd result)
    |> List.tryHead
    |> Option.defaultValue (None, [])

