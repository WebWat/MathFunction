open System
open System.Diagnostics
open Node
open Simplify
open Limit
open Derivative
open CompMath

while true do
    printf "\nInput function: "
    let text = Console.ReadLine() // x*x-x*2-x*3-(2*x-2*2-2*3)-(3*x-3*2-3*3) x^2+x+3-2+2^x+x^2
    let der = convertToFunc text
    printfn "string: %s" (der.ToString())
    printfn "Derivative string: %s" ((simplifyFunc der).ToString())
    //if text = "clear" then
    //    let start = (float (GC.GetTotalMemory(false)) / (1024. * 1024.))
    //    printfn "Before: %.1f KB" start

    //    GC.Collect();
    //    GC.WaitForPendingFinalizers();

    //    let _end = (float (GC.GetTotalMemory(false)) / (1024. * 1024.))
    //    printfn "After: %.1f KB" _end

    //    printfn "Free: %.1f KB" (start - _end)

    //else
    //    let l = new Stopwatch()

    //    l.Start()

    //    try
    //        let func = convertToFunc text
    //        //let der = derivativeFunc func

    //        printfn "Function string: %s" (func.ToString())
    //        //printfn "Derivative string: %s" (der.ToString())

    //        //for i in -5.0..0.5..5.0 do
    //        //    printf "y(%f) = %f | " i (calculateFunc func i)

    //        //    try
    //        //        printf "y' = %f | " (derivative func i)
    //        //    with
    //        //        | NotExist -> printf "y' = Not exist | "

    //        //    printfn "y'alpha = %f" (calculateFunc der i)
    //    with
    //    | ex -> printfn "Incorrect input: %s" ex.Message

    //    l.Stop()

    //    printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)
