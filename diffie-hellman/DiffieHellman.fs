module DiffieHellman

open System

let privateKey primeP = 
    Random.Shared.NextInt64() |> bigint |> (%) primeP |> (-) 1I

let publicKey primeP primeG privateKey = failwith "You need to implement this function."

let secret primeP publicKey privateKey = failwith "You need to implement this function."