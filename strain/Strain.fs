module Seq

let rec keep pred xs = 
    [for x in xs do  
        if pred x then
            yield x]

let discard pred xs = 
    keep (pred >> not) xs

