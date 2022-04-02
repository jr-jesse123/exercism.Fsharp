module Leap

let isEvenlyDivisibleBy input divisor =
    input % divisor = 0
let (%%%) = isEvenlyDivisibleBy

let leapYear (year: int): bool = 
    match year %%% 4 with
    | true -> 
        match year %%% 100 with
        | true -> year %%% 400
        | false -> true
    | false -> false

