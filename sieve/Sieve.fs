module Sieve

open    System

let utilRange = [1..1000]

let getMultiplesof x = 
    [1..1000]
    |> List.map ((*) x)

let notPrimes = 
    [1..1000]
    |> List.map (getMultiplesof)
    |> List.map (List.skip 1)
    |> List.groupBy (List.head)
    |> List.skip 1
    |> List.map snd
    |> List.map List.concat    
    |> List.concat    
    |> List.sort
    |> List.distinct




let primes limit = 
    [2..limit]
    |> List.except notPrimes


    
    