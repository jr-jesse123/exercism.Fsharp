module CustomSet

// TODO: define the Set type

// The simplest way to implement a custom set that's actually
// efficient is with an existing data type like Map. The set
// items are keys, and we don't care about the values. And for
// anything we don't care about, the right value is () (unit).
// let empty = Map.empty
// let isEmpty set = Map.isEmpty set
// let insert item set = set |> Map.add item ()
// let singleton item = empty |> insert item
// let contains = Map.containsKey
// // Note that these functions are just as easy as in the list
// // implementation. Yet they run quite a lot faster.
// let fromList lst =
//     lst |> List.fold (fun map item -> insert item map) empty
// let isSubsetOf left right =
//     left |> Map.forall (fun x _ -> right |> contains x)
// let union left right =
//     Map.fold (fun acc x _ -> insert x acc) left right
// let intersection left right =
//     left |> Map.filter (fun x _ -> right |> contains x)
// let difference left right =
//     left |> Map.filter (fun x _ -> right |> contains x |> not)
// let isDisjointFrom left right =
//     intersection left right |> isEmpty
[<AutoOpen>]
module CustomSet =
    let isEqualTo a b = a = b

    let empty  = Map.empty


    let singleton value = failwith "You need to implement this function."

    let isEmpty set = 
        Seq.length  (Map.keys set) = 0

    let size set = failwith "You need to implement this function."

    let fromList list = 
        let mutable map = empty //TODO: remove mutable
        List.iter (fun x -> map <- map.Add(x,()) ) list
        map

    let toList set = failwith "You need to implement this function."

    let contains value set = 
        Map.containsKey value set

    let insert value (set: Map<'a,unit>) = 
        set.Add(value,())

    let union (left: Map<int,unit>) (right: Map<int,unit>) = 
        left.Keys
        |> Seq.append right.Keys
        |> Seq.map (fun x -> x, ())
        |> Map.ofSeq


    let intersection (left: Map<'a,unit>) right = 
        Map.filter (fun key value -> left.ContainsKey key) right
        
    let difference (left: Map<int,unit>) (right: Map<int,unit>) : Map<int,unit>= 
        Map.filter (fun key value -> right.ContainsKey key |> not) left
        
        
        
    let isSubsetOf left right = 
        let left = Map.keys left
        let right = Map.keys right

        Seq.map (fun x -> Seq.contains x right) left 
        |> Seq.fold (&&) true

    let isDisjointFrom left right = 
        let left = Map.keys left
        let right = Map.keys right

        Seq.map (fun x -> Seq.contains x right) left 
        |> Seq.contains (true) 
        |> not