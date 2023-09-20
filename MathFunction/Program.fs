open System
open System.Diagnostics
open MathFunction


let _default (text: string) : unit =
    if text = "clear" then
        let start = (float (GC.GetTotalMemory(false)) / (1024. * 1024.))
        printfn "Before: %.1f KB" start

        GC.Collect();
        GC.WaitForPendingFinalizers();

        let _end = (float (GC.GetTotalMemory(false)) / (1024. * 1024.))
        printfn "After: %.1f KB" _end

        printfn "Free: %.1f KB" (start - _end)
    else
        let stopwatch = new Stopwatch()

        stopwatch.Start()

        let func = FunctionX text
        let der = func.Derivative()

        printfn "Function string: %s" (func.ToString())
        //printfn "MathJax string: %s" (func.ToMathJax())
        printfn "Derivative string: %s" (der.ToString())
        //printfn "MathJax string: %s" (der.ToMathJax())

        for i in -1.0..0.2..1.0 do
            printf "y(%f) = %f | " i (func.Calc i)
            printf "y'(%f) = %f | " i (der.Calc i)

        stopwatch.Stop()

        printfn "Time: %.3f s" (float stopwatch.ElapsedMilliseconds * 1e-3)

while true do
    printf "\nInput function: "

    let text = Console.ReadLine()
    let func = MultivariableFunction text
    
    _default text




