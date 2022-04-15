module BankAccount
open System

type State = 
| Open 
| Closed

type BalanceMsg = 
| GetBalance of AsyncReplyChannel<Decimal>
| ChangeBalance of Decimal

type AccountInfo ()  = 
    let mutable History : decimal List = []
    let mutable state = Closed
    let getBalance() = History |> List.sum
    let updateBalance value  =  History <- value::History

    let balanceAgent = MailboxProcessor.Start(fun inbox ->
        let rec innerLoop() = async{
            match! inbox.Receive() with
            | GetBalance repplyChannel -> repplyChannel.Reply (getBalance())
            | ChangeBalance value -> updateBalance value

            return! innerLoop()
        }
        innerLoop()

    )

    member _.Id = Guid.NewGuid()
    member _.Balance ()  = 
        balanceAgent.PostAndReply GetBalance

    static member Create () =  AccountInfo()

    member this.UpdateBalance value  = 
        balanceAgent.Post (ChangeBalance value)
        this

    member this.Open = 
        state<- Open
        this
    
    member this.Close  = 
        state <- Closed
        this

    member _.State = state

    

let mkBankAccount() = AccountInfo.Create()

let openAccount (account: AccountInfo) = 
    match account.State with
    | Closed ->  account.Open
    | _ -> failwith "can´t open already opened account"

let closeAccount (account: AccountInfo) = 
    account.Close

let getBalance (account: AccountInfo) = 
    match account.State with
    | Open  -> Some (account.Balance ())
    | Closed -> None


let updateBalance change (account: AccountInfo) = 
    match account.State with
    | Open  ->  (account.UpdateBalance  change)
    | Closed -> failwith ""
