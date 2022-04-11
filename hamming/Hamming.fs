module Hamming

let calculateHammingDistance (strand1: string) (strand2: string) = 
    
    if strand1.Length <> strand2.Length then failwith "only euqal length strands are allowed"
    Array.zip (strand1.ToCharArray ()) (strand2.ToCharArray ()) 
    |> Array.map (fun (c1, c2) -> c1 <> c2) 
    |> Array.filter id
    |> Array.length

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length <> strand2.Length then None 
        else 
            calculateHammingDistance strand1 strand2 |> Some