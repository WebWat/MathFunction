open System
open System.Diagnostics
open Node
open Derivative
open CompMath

while true do
    printf "\nInput function: "
    let text = Console.ReadLine()

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
        let func = FunctionX text
            //let der = func.Deravative()

        printfn "Function string: %s" (func.ToString())
        //printfn "Deravative string: %s" (der.ToString())

        for i in -5.0..0.5..5.0 do
            printfn "y(%f) = %f | " i (func.Calc i)
        //try
        //    let func = FunctionX text
        //    //let der = func.Deravative()

        //    printfn "Function string: %s" (func.ToString())
        //    //printfn "Deravative string: %s" (der.ToString())

        //    for i in -5.0..0.5..5.0 do
        //        printfn "y(%f) = %f | " i (func.Calc i)
        //        //printfn "y'(%f) = %f | " i (der.Calc i)
        //with
        //| ex -> printfn "Incorrect input: %s" ex.Message

        l.Stop()

        printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)
