module SpaceAge
type Planet = 
|Mercury //: orbital period 0.2408467 Earth years
|Venus //: orbital period 0.61519726 Earth years
|Earth //: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
|Mars //: orbital period 1.8808158 Earth years
|Jupiter //: orbital period 11.862615 Earth years
|Saturn //: orbital period 29.447498 Earth years
|Uranus //: orbital period 84.016846 Earth years
|Neptune //: orbital period 164.79132 Earth years

let getOrbitalPeriod (rate: float) = 
    1.0 / rate

let getRate =
    function
    |Mercury -> 0.2408467// Earth years
    |Venus ->0.61519726// Earth years
    |Earth ->1.0 //Earth years, 365.25 Earth days, or 31557600 seconds
    |Mars ->1.8808158 //Earth years
    |Jupiter ->11.862615 //Earth years
    |Saturn ->29.447498 //Earth years
    |Uranus ->84.016846 //Earth years
    |Neptune ->  164.79132

let secondsToYear seconds = 
    seconds / 31557600.0

let age (planet: Planet) (seconds: int64): float = 
    planet 
    |> getRate 
    |> getOrbitalPeriod 
    |> (*) (float seconds)
    |> secondsToYear
    |> float
    