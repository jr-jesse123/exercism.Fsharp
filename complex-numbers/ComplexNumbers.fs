module ComplexNumbers

type ComplexN = {Real: float; Imaginary:float}

let create real imaginary = 
    {Real=real;Imaginary=imaginary}

let mul z1 z2 = 
    let realPart = (z1.Real * z2.Real) - (z1.Imaginary * z2.Imaginary)
    let imaginaryPart = (z1.Real * z2.Imaginary) + (z1.Imaginary * z2.Real)
    { Real = realPart; Imaginary = imaginaryPart }
    

let add z1 z2 = 
    { Real = z1.Real + z2.Real; Imaginary = z1.Imaginary + z2.Imaginary }
    

let sub z1 z2 = 
    { Real = z1.Real - z2.Real; Imaginary = z1.Imaginary - z2.Imaginary }


let div z1 z2 = 
    let denominator = (z2.Real ** 2.0) + (z2.Imaginary ** 2.0)
    let realPart = ((z1.Real * z2.Real) + (z1.Imaginary * z2.Imaginary)) / denominator
    let imaginaryPart = ((z1.Imaginary * z2.Real) - (z1.Real * z2.Imaginary)) / denominator
    { Real = realPart; Imaginary = imaginaryPart }
    

let abs z = 
    sqrt (z.Real ** 2.0 + z.Imaginary ** 2.0)
   

let conjugate z = 
    { Real = z.Real; Imaginary = -z.Imaginary }
    

let real ({Real=z}) = z
    

let imaginary ({Imaginary=z}) = z

let exp z = 
    let ea = exp z.Real  // Calcula e^a
    let cosB = cos z.Imaginary  // Calcula cos(b)
    let sinB = sin z.Imaginary  // Calcula sin(b)
    { Real = ea * cosB; Imaginary = ea * sinB }
