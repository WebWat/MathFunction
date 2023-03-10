module Limits

open System
open CompMath

exception NotExist

type Position =
    | Left
    | Right

let limit (func: float -> float) (x: float) (accuracy: int) (position: Position) =   
    if x = infinity || x = -infinity then raise (NotImplementedException())
    if accuracy < 0 then raise (ArgumentException("Accuracy cannot be less than 0 digits?"))

    let result = func x

    if result = infinity || Double.IsNaN result then
        let needed = pown 10. -accuracy

        let rec calculate eps ldiff mayBeInf =
            // Take two points on the interval
            let (first, second) = 
                 match position with
                 | Position.Left -> (func (x - eps * 2.), func (x - eps))
                 | Position.Right -> (func (x + eps * 2.), func (x + eps))
            
            let diff = abs(second - first)

            // If there is no function
            if Double.IsNaN diff then raise (NotExist)

            //printfn "\nStart => %.20f\nEnd => %.20f\nDiff => %.20f\nEps  => %.20f\nldiff => %.20f" first second diff eps ldiff
            
            // If the value of the function does not change
            if diff = 0 then second
            // If reached the specified accuracy OR the calculations are inconsistent
            elif diff < needed then second
            elif diff > ldiff then
                if mayBeInf = true then 
                    if second > first then infinity else -infinity
                else
                    second
            else calculate (eps * 0.1) diff false

        let startDiff = abs(func (x - 0.2) - func (x - 0.1))

        if Double.IsNaN startDiff then raise (NotExist)

        calculate 0.01 startDiff true
    else
        result

let derivative (func: FunctionX) (point: float) =
    limit (fun x -> ((func.Calc(point + x) - func.Calc(point))/ x)) 0 6 Position.Left

