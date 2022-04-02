module PizzaPricing

type Pizza = 
| Margherita
| Caprese
| Formaggio
| ExtraSauce of Pizza
| ExtraToppings of Pizza

let rec pizzaPrice (pizza: Pizza): int = 
    match pizza with
    | Margherita -> 7
    | Caprese -> 9
    | Formaggio -> 10
    | ExtraSauce  pizza -> 1 + pizzaPrice pizza
    | ExtraToppings pizza -> 2 + pizzaPrice pizza

    
let orderPrice(pizzas: Pizza list): int = 
    match pizzas with
    | [pizza] -> 3 + pizzaPrice pizza
    | [pizza1; pizza2] ->  pizzas |> List.map pizzaPrice |> List.sum |> (+) 2
    | _ -> pizzas |> List.map pizzaPrice |> List.sum