module Allergies

open System

[<Flags>]
type Allergen =
    |Eggs = 1 // (1)
    |Peanuts = 2 // (2)
    |Shellfish = 4 // (4)
    |Strawberries = 8 // (8)
    |Tomatoes = 16 // (16)
    |Chocolate =32// (32)
    |Pollen =64// (64)
    |Cats =128// (128)


let allergicTo (codedAllergies: int) allergen = 
    let alergies = enum<Allergen>(codedAllergies)
    alergies.HasFlag allergen



let getFlags (enum: Allergen) = 
    Enum.GetValues(enum.GetType())
    |> Seq.cast<Allergen>
    |> Seq.filter enum.HasFlag

let list codedAllergies = 

    enum<Allergen>(codedAllergies)
    |> getFlags 
    |> Seq.toList
