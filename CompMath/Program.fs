open System
open System.Diagnostics
open Node
open Expand
open Derivative
open CompMath

while true do
    printf "\nInput function: "
    let text = Console.ReadLine() 

    //let der = convertToFunc text
    //printfn "string: %s" (der.ToString())

    if text = "clear" then
        let start = (float (GC.GetTotalMemory(false)) / (1024. * 1024.))
        printfn "Before: %.1f KB" start

        GC.Collect();
        GC.WaitForPendingFinalizers();

        let _end = (float (GC.GetTotalMemory(false)) / (1024. * 1024.))
        printfn "After: %.1f KB" _end

        printfn "Free: %.1f KB" (start - _end)

    else
        let l = new Stopwatch()

        l.Start()

        try
            let func = FunctionX text

            printfn "Function string: %s" (func.ToString())

            for i in -1.0..0.5..1.0 do
                printfn "y(%f) = %f | " i (func.Calc i)
        with
        | ex -> printfn "Incorrect input: %s" ex.Message

        l.Stop()

        printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)
