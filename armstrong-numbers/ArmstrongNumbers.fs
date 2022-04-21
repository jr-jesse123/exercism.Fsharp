module ArmstrongNumbers

let (^^) a b = System.Math.Pow(float a,float b)


let isArmstrongNumber (number: int) (*: bool*) = 
    let digits = 
        number
            .ToString()
            .ToCharArray() 
        |> Array.map (string >> int)
    
    Array.map (fun d -> d ^^ digits.Length  ) digits
    |> Array.sum 
    |> (=) number


