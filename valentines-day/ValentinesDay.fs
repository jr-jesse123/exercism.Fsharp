module ValentinesDay



type Approval = 
| Yes
| No
| Maybe


type Cuisine=
| Korean
| Turkish


type Genre = 
| Crime
| Horror
| Romance 
| Thriller

type Activity = 
| BoardGame
| Chill
| Movie of Genre
| Restaurant of Cuisine
| Walk of miles:int

let rateActivity (activity: Activity): Approval =
    match activity with
    | BoardGame 
    | Chill -> No
    | Restaurant (cousine) -> 
        match cousine with
        | Korean -> Yes
        | Turkish -> Maybe
    | Movie genre ->    
        match genre with
        | Romance -> Yes
        | _ -> No
    | Walk distance when distance < 3 -> Yes
    | Walk distance when distance < 5 -> Maybe
    | _ -> No