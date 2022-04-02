module InterestIsInteresting
open System

let interestRate (balance: decimal): single =
    match balance with
    | b when b < 0M -> 3.213f
    | b when b < 1000M -> 0.5f
    | b when b < 5000M -> 1.621f
    | _ -> 2.475f

let interest (balance: decimal): decimal =
   let rate = balance |> interestRate
   decimal rate / 100M * ( balance)

let annualBalanceUpdate(balance: decimal): decimal =
   interest balance + balance

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
   match balance with 
   | b when b < 0M -> 0
   | b -> 
      let amount = decimal taxFreePercentage * 2.0M * balance /100M
      Math.Round(amount,0,MidpointRounding.ToNegativeInfinity) |> int