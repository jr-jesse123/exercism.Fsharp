module Clock
// type QuocientAndRemaing = {Quocient:int;Remaing:int}

let between begining final input = input >= begining && input <= final 


module Clock = 
    let getIntAndMod input divisor =
        let Quocient = input / divisor
        let Remaing = input % divisor
        Quocient , Remaing

    let (|/|) = getIntAndMod 

    let ``Raw Minutes to hours with minutes`` min = 
        let hours, minutesLeft = min |/| 60
        let days , hoursLeft = hours |/| 24
        hoursLeft , minutesLeft
    
    let discartDays min = ``Raw Minutes to hours with minutes`` min |> fun (hours, minutes) -> (hours  * 60) + minutes

    let (|NegativeInt|_|) nr = 
        if nr <0 then Some nr else None
    [<CustomEquality>]
    [<CustomComparison>]
    type  Clock = private Clock of TotalMinutes:int 
    with
        member internal this.value = 
            let (Clock min) = this 
            

            match discartDays min  with
            | NegativeInt nr -> nr + (24 * 60) // gets to the hour of the day before
            | nr -> nr
        
        member internal this.Add minutes = 
            Clock (this.value + minutes)

        member this.Hours = 
            match ``Raw Minutes to hours with minutes`` this.value |> fst with
            //| NegativeInt nr -> nr + 24
            | nr -> nr
        member this.Minutes' = 
            match ``Raw Minutes to hours with minutes`` this.value |> snd with
            //| NegativeInt nr -> nr + 60
            | nr -> nr
        
        override this.Equals(other : obj) = 
            match other with
            | :? Clock as other -> ( discartDays other.value ) = (discartDays this.value)
            | _ -> false    

        override this.GetHashCode () = 
            this.value.GetHashCode()

        interface System.IComparable with
            member this.CompareTo (obj:obj) = 
                let other = obj :?> Clock
                match other.value , this.value with
                | other , this when other > this -> -1
                | _ -> 1


    let add  minutes (clock: Clock) = 
        //let minutes = (hours * 60) + minutes
        // let (hours , minutsLeft) =  + minutes + clock.value |/| 60
        // let  ( days ,hoursLeft) = hours  |/| 24
        //Clock (hoursLeft + minutsLeft)
        clock.Add minutes
        
    let create hour min =
        Clock ((hour * 60) + min)
        
        
        
let create hours minutes = // Clock.Create hours minutes
        Clock.create hours minutes  

let add minutes clock = Clock.add  minutes clock

let subtract minutes clock = Clock.add  (- minutes) clock


let display (clock: Clock.Clock) = 
    sprintf "%s:%s" (clock.Hours.ToString("00")) (clock.Minutes'.ToString("00"))



let clock1 = create 10 37
let clock2 = create 34 37

clock1 = clock2 