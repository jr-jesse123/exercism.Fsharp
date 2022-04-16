module BinarySearchTree

type BST = {
    value:int
    mutable left:BST option
    mutable right:BST option
}
module BST = 
    let fromValue v = {value=v;left=None;right=None}

let left node  = 
    node.left

let right node = 
    node.right

let data node = 
    node.value

type leftRight =
|Left 
|Right

let toLeft_Right vbase vnode = 
    if vnode <= vbase then Left else Right

let rec FindNodeWithEmptySlot nodeBase value =
    match toLeft_Right nodeBase.value value with
    | Left when nodeBase.left.IsNone -> nodeBase , Left
    | Left when nodeBase.left.IsSome -> FindNodeWithEmptySlot nodeBase.left.Value value
    | Right when nodeBase.right.IsNone -> nodeBase , Right
    | Right  when nodeBase.right.IsSome -> FindNodeWithEmptySlot nodeBase.right.Value value
    | _ -> failwith ""



let  create (items: int list) =
    let baseNode = BST.fromValue items[0]
 
    for item in items[1..] do
        let suitableNode = FindNodeWithEmptySlot baseNode item
        match suitableNode with
        |node, Left ->  node.left <- BST.fromValue item |> Some
        |node, Right -> node.right <- BST.fromValue item |> Some

    baseNode

let rec toList node = 
    [
        yield!
            match node.left with
            |Some leftNode-> toList leftNode
            |None ->  []
        
        node.value 
        
        yield!
            match node.right with
            |Some righNode-> toList righNode
            |None ->  []
        
    ]

let sortedData node = 
    toList node |> List.sort

