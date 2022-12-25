open System
open System.Diagnostics
open Function
open Limit

while true do
    printf "\nInput function: "
    let text = Console.ReadLine() //1/(x*cos(2*x))

    if text = "clear" then
        printfn "Before: %.1f KB" (float (GC.GetTotalMemory(false)) / (1024. * 1024.))

        GC.Collect();
        GC.WaitForPendingFinalizers();

        printfn "After %.1f KB" (float (GC.GetTotalMemory(false)) / (1024. * 1024.))

    else
        let l = new Stopwatch()

        l.Start()

        try
            let func = convertToFunc text
            let der = simplifyFunc (derivativeFunc func)

            printfn "Function string: %s" (func.ToString())
            printfn "Derivative string: %s" (der.ToString())

            for i in -5.0..0.5..5.0 do
                printf "y(%f) = %f | " i (calculateFunc func i)

                try
                    printf "y' = %f | " (derivative func i)
                with
                    | NotExist -> printf "Not exist | "

                printfn "y'alpha = %f"(calculateFunc der i)
        with
        | ex -> printfn "Incorrect input: %s" ex.Message

        l.Stop()

        printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)
