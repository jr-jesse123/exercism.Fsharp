module BinarySearchTree

type Tree =
| Empty 
| Node of value:int * left:Tree * right:Tree

[<AutoOpen>]
module Tree = 
    let fromValue value = Node (value,Empty,Empty)
    let right node =
        match node with
        | Empty | Node (_,_,Empty) -> None
        | Node (_,_,r) -> Some r

    let left node =
        match node with
        | Empty | Node (_,Empty,_) -> None
        | Node (_,l,_) -> Some l

    let data = function
    | Empty -> failwith ""
    | Node (v,l,r) -> v
    

let create items =
    let rec insert item node = 
        match node with
        | Empty -> Tree.fromValue item
        | Node (v,l,r) when item > v -> Node (v,l,insert item r)
        | Node (v,l,r) -> Node (v,insert item l, r)
    List.fold (fun tree item -> insert item tree) Empty items
    


let rec sortedData node =
    match node with
    | Empty -> []
    | Node (v,l,r) -> sortedData l @ [v] @ sortedData r

