module BankAccount
open System
type AccountInfo = 
    {
        Id:Guid 
        History: Decimal List
    }
with 
    member this.Balance = this.History |> List.sum
    static member Create () = Closed {Id=Guid.NewGuid() ; History = []}
    member this.UpdateBalance value = {this with History = value::this.History}

and Account = 
| Open of AccountInfo
| Closed of AccountInfo

let mkBankAccount() = AccountInfo.Create()

let openAccount account = 
    match account with
    | Closed acc -> Open acc
    | _ -> failwith "can´t open already opened account"

let closeAccount account = failwith "You need to implement this function."

let getBalance account = 
    match account with
    | Open acc -> Some acc.Balance   
    | _ -> None
let updateBalance change account = 
    match account with
    | Open acc -> Some (acc.UpdateBalance change)
    | Closed acc -> None
