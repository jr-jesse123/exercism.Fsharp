module BankAccount
open System

type State = 
| Open 
| Closed

type BalanceMsg = 
| GetBalance of AsyncReplyChannel<Decimal>
| ChangeBalance of Decimal

type AccountInfo  = 
    {
        Id:Guid 
        mutable History: Decimal List
        mutable State: State
    }
// with 
//     val  private  teste:int
//     member this.MyReadWriteProperty
//         with get () = this.teste
//         and set (value) = this.teste <- value
        
    
module AccountInfo = 
    let Balance this = this.History |> List.sum
    let Create () =  {Id=Guid.NewGuid() ; History = [] ; State=Closed}
    let UpdateBalance value this = 
        this.History <- value::this.History
        this

    let Open this = {this with State= Open}
    let Close this = {this with State = Closed}

let mkBankAccount() = AccountInfo.Create()

let openAccount account = 
    match account.State with
    | Closed -> AccountInfo.Open account
    | _ -> failwith "can´t open already opened account"

let closeAccount account = 
    AccountInfo.Close account

let getBalance account = 
    match account.State with
    | Open  -> Some (AccountInfo.Balance account)
    | Closed -> None


let updateBalance change account = 
    match account.State with
    | Open  ->  (AccountInfo.UpdateBalance change account)
    | Closed -> failwith ""
