module BankAccount
open System

type Message = 
| OpenAccount
| CloseAccount
| GetBalance of AsyncReplyChannel<decimal option>
| UpdateBalance of decimal
    
type Account = MailboxProcessor<Message>

let processor (account:Account) =
    let rec loop balance = async{
        let! message = account.Receive()
        match message with
        | OpenAccount -> return! loop (Some 0.0M)
        | CloseAccount -> return! loop None
        | GetBalance channel -> channel.Reply balance ; return! loop balance
        | UpdateBalance change -> return! loop (balance |> Option.map((+) change))
    }
    loop None

let mkBankAccount() = 
    MailboxProcessor.Start(fun account -> processor account )
    
let openAccount (account: Account) = 
    account.Post OpenAccount; account

let closeAccount (account: Account) = 
    account.Post CloseAccount
    account

let getBalance (account: Account) = 
    let replly = account.PostAndAsyncReply GetBalance 
    Async.RunSynchronously replly

let updateBalance change (account: Account) = 
    account.Post (UpdateBalance change); account
