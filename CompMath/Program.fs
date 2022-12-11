module Main

open System

type Position =
    | Left
    | Right

type Value = 
    | Infinity
    | NegitiveInfinity
    | Default of value: float

let limit (func: float -> float) (x: float) (position: Position) =
    let result = func x

    let mutable none = 0

    if result = infinity || not (Int32.TryParse(result.ToString(), &none)) then
        let result = 0.0

        let eps = 1e-3
        //let eps = 1e-3

        let (st, nd) = 
            match position with
            | Position.Left -> (func (x - eps * 2.0), func (x - eps))
            | Position.Right -> (func (x + eps * 2.0), func (x + eps))
        
        printfn "Start => %.20f\nEnd => %.20f" st nd

        if abs(st - nd) < eps then nd
        elif nd > st then infinity else -infinity
    else
        result

printfn "%f" (limit (fun x -> 1.0 / x) 0 Position.Left)
printfn "%f" (limit (fun x -> 1.0 / x) 0 Position.Right)

printfn "%f" (limit (fun x -> (x - 5.0) / abs(x - 5.0)) 5 Position.Right)
printfn "%f" (limit (fun x -> (x - 5.0) / abs(x - 5.0)) 5 Position.Left)

printfn "%f" (limit (fun x -> sin(x) / x) 0 Position.Right)
printfn "%f" (limit (fun x -> sin(x) / x) 0 Position.Left)

printfn "%f" (limit (fun x -> x * x / (1.0 - x * x)) 1 Position.Right)
printfn "%f" (limit (fun x -> x * x / (1.0 - x * x)) 1 Position.Left)

printfn "%f" (Math.PI / 2.0)

printfn "%f" (limit (fun x -> Math.Atan(1.0 / (1.0 - x))) 1 Position.Left)
printfn "%f" (limit (fun x -> Math.Atan(1.0 / (1.0 - x))) 1 Position.Right)

printfn "%f" (limit (fun x -> (sin(2.0 * x) - 2.0 * sin x)/(x * log(cos(5.0 * x)))) 0 Position.Left)
Console.ReadLine() |> ignore
