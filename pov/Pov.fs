module Pov

type Graph<'a> = { value: 'a; children: Graph<'a> list }

let mkGraph value children = { value = value; children = children }



let graphPath toValue graph = 
    let rec aux  p g = 
        if g.value = toValue then Some (g :: p)
        else
            let p' = g :: p
            //List.tryFind Option.isSome (List.map (fun g' -> aux g' p') g.children)
            //List.tryPick  ((fun g' -> aux g' p') ) g.children
            List.tryPick  (aux p')  g.children
            //|> Option.flatten
    aux [] graph

let rec reconstruct graph remainingPath  = 
    match remainingPath with
    | [] -> graph
    |  p :: ps' ->
        let p' = 
            {p with children = List.filter (fun x -> x.value <> graph.value) p.children}
        {graph with children = (reconstruct p' ps') :: graph.children  }
    
    //Option.map (fun p -> reconstruct (List.head p) (List.tail p)) path




let fromPOV ofValue graph = 
    let path = graphPath ofValue graph
    Option.map (fun p-> reconstruct (List.head p) (List.tail p) ) path

let tracePathBetween from too graph = 
    let g' = fromPOV from graph
    let path = Option.map (graphPath too) g' |> Option.flatten
    Option.map (List.rev >> List.map (fun g -> g.value)) path

