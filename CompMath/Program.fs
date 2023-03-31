open System
open System.Diagnostics
open Node
open Derivative
open Limits
open CompMath


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
        let l = new Stopwatch()

        l.Start()

        let func = FunctionX text
        let der = func.Derivative()

        printfn "Function string: %s" (func.ToString())
        //printfn "MathJax string: %s" (func.ToMathJax())
        printfn "Derivative string: %s" (der.ToString())
        //printfn "MathJax string: %s" (der.ToMathJax())

        for i in -1.0..0.2..1.0 do
            printf "y(%f) = %f | " i (func.Calc i)
            printf "y'beta(%f) = %f | " i (der.Calc i)
            try
                printfn "y'(%f) = %f | " i (derivative func i)
            with
            | _ -> printfn "Not exist"

        l.Stop()

        printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)

while true do
    // ln(cos(x)*sin(x))
    printf "\nInput function: "
    let text = Console.ReadLine()
    let func = FunctionMult text
    let args = func.ArgsCount()
    printfn "%A" args

    let mutable floors: float array = [||]

    for elem in args do
        printf "%c: " elem
        floors <- Array.append floors [|(float(Console.ReadLine()))|]

    let map = [|for i = 0 to args.Length - 1 do i|] |> Array.map (fun x -> (args[x], floors[x])) |> Map.ofArray

    printfn "Map: %A\nResult: %f" map (func.Calc map)




