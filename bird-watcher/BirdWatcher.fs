module BirdWatcher
open System

let lastWeek: int[] =
   [|0;2;5;3;7;8;4|]

let yesterday(counts: int[]): int =
  counts[5]

let total(counts: int[]): int =
  counts |> Array.sum

let dayWithoutBirds(counts: int[]): bool =
  counts |> Array.contains 0

let incrementTodaysCount(counts: int[]): int[] =
  counts[6] <- counts[6] + 1
  counts

let isEvenDay = fun i -> (i + 1) % 2 = 0
let isExcactly = (=)
let isOddDay = isEvenDay >> not


let mapWeek dayfilter predicate counts = 
  counts 
  |> Array.mapi (fun index count -> dayfilter index , predicate count)
  |> Array.filter fst
  |> Array.map snd
  |> Array.reduce (&&)


let noBirdsOnEvenDays  =  mapWeek isEvenDay (isExcactly 0) 
  
let tenBirdsInEvenDays  = mapWeek isEvenDay (isExcactly 10) 

let fiveBirdsOnOddDays = mapWeek isOddDay (isExcactly 5)

let oddWeek(counts: int[]): bool = 
  noBirdsOnEvenDays counts || tenBirdsInEvenDays counts || fiveBirdsOnOddDays counts