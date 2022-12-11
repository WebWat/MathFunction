module Main

open System

exception DontExist

type Position =
    | Left
    | Right


let limit (func: float -> float) (x: float) (accuracy: int) (position: Position) =          
    let result = func x

    if result = infinity || Double.IsNaN result then
        let needed = pown 10. -accuracy

        let rec calculate approach eps ldiff =
            // Take two points on the interval
            let (first, second) = 
                 match position with
                 | Position.Left -> (func (x - eps * 2.0), func (x - eps))
                 | Position.Right -> (func (x + eps * 2.0), func (x + eps))
            
            let diff = abs(second - first)

            // If there is no function
            if Double.IsNaN diff then raise (DontExist)

            printfn "\nStart => %.20f\nEnd => %.20f\nDiff => %.20f\nEps  => %.20f\nldiff => %.20f" first second diff eps ldiff
            
            // If the value of the function does not change
            if diff = 0 then second
            // If reached the specified accuracy OR the calculations are inconsistent
            elif diff < needed || diff > ldiff then approach
            // If the values tend to infinity
            elif diff > eps * 10. then
                if second > first then infinity else -infinity
            else calculate second (eps * 0.1) diff

        calculate 0.0 0.1 10
    else
        result

//printfn "%f" (limit (fun x -> 1.0 / x) 0 3 Position.Left)
//printfn "%f" (limit (fun x -> 1.0 / x) 0 3 Position.Right)
//printfn "%f" (limit (fun x -> x * x) 2 3 Position.Left)

//printfn "%f" (limit (fun x -> (x - 5.0) / abs(x - 5.0)) 5 10 Position.Left)
//printfn "%f" (limit (fun x -> (x - 5.0) / abs(x - 5.0)) 5 Position.Left)

//printfn "%f" (limit (fun x -> sin(x) / x) 0 Position.Right)
//printfn "%f" (limit (fun x -> sin(x) / x) 0 Position.Left)

//printfn "%f" (limit (fun x -> x * x / (1.0 - x * x)) 1 Position.Right)
//printfn "%f" (limit (fun x -> x * x / (1.0 - x * x)) 1 3 Position.Right)


//printfn "%.30f" (limit (fun x -> Math.Atan(1.0 / (1.0 - x))) 1.0 10 Position.Left)
//printfn "%.30f" (Math.PI / 2.0)
//printfn "%f" (limit (fun x -> Math.Atan(1.0 / (1.0 - x))) 1 Position.Right)

printfn "%f" (limit (fun x -> (sin(2.0 * x) - 2.0 * sin x)/(x * log(cos(5.0 * x)))) 0 20 Position.Left)
printfn "%.20f" (limit (fun x -> (sqrt(cos x) - Math.Pow(cos x, 1./3.))/pown (Math.Atan(x)) 2) 0 20 Position.Left)
Console.ReadLine() |> ignore
