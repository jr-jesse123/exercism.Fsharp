module Pov

type Graph<'a> = { value: 'a; children: Graph<'a> list }

let mkGraph value children = { value = value; children = children }

let graphPath toValue graph =
    let rec aux g p =
        if g.value = toValue then Some (g :: p)
        else 
            let p' = g :: p
            List.tryFind Option.isSome (List.map (fun g' -> aux g' p') g.children)
            |> Option.flatten
    aux graph []

let fromPOV ofValue graph =
    let path = graphPath ofValue graph
    let rec reconstruct g ps =
        match ps with
        | [] -> g
        | p :: ps' -> 
            let p' = { p with children = List.filter (fun x -> x.value <> g.value) p.children }
            { g with children = (reconstruct p' ps') :: g.children }
    Option.map (fun p -> reconstruct (List.head p) (List.tail p)) path

let tracePathBetween fromValue toValue graph =
    let g' = fromPOV fromValue graph
    let path = Option.map (graphPath toValue) g' |> Option.flatten
    Option.map (List.rev >> List.map (fun g -> g.value)) path

let rec graphToList (graph: Graph<'a>) = 
    let right =
        graph.children
        |> List.sortBy (fun x -> x.value)
        |> List.collect graphToList
    [graph.value] @ right