module NthPrime



let prime nth : int option = 
    match nth with
    | 0 -> None
    | n ->
        let isprime n =
            let rec check i =
                i > n/2 || (n % i <> 0 && check (i + 1))
            check 2
        seq { for n in 2..System.Int32.MaxValue do if isprime n then n }
        |> Seq.take n
        |> Seq.last
        |> Some

