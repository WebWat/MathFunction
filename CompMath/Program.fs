open System
open System.Diagnostics
open Node
open Expand
open Limit
open Derivative
open CompMath

while true do
    printf "\nInput function: "
    let text = Console.ReadLine() // (1*(x+1)+x*(1+0))*cos(x*(x+1))
    //let der = convertToFunc text
    //// (x*x^2)^(2)
    //printfn "string: %s" (der.ToString())
    //printfn "Derivative string: %s" ((multiplyAll der).ToString())
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
            let func = convertToFunc text
            let der = derivativeFunc func
            let exp = multiplyAll der

            printfn "Function string: %s" (func.ToString())
            printfn "Derivative string: %s" (der.ToString())
            printfn "Expand string: %s" (exp.ToString())

            for i in -5.0..0.5..5.0 do
                printf "y(%f) = %f | " i (calculateFunc func i)

                try
                    printf "y' = %f | " (derivative func i)
                with
                    | NotExist -> printf "y' = Not exist | "

                printfn "y'alpha = %f" (calculateFunc exp i)
        with
        | ex -> printfn "Incorrect input: %s" ex.Message

        l.Stop()

        printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)
