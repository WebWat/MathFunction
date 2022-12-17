open System
open System.Diagnostics
open Function
open Limit

Debug.WriteLine("asd")

while true do
    let l = new Stopwatch()

    printf "\nInput function: "
    // |x^x+|ln(tg(x+2))||*(x+55)
    let text = Console.ReadLine()

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
            printfn "String result: %s" (func.ToString())
            for i in -5.0..0.5..5.0 do
                printfn "y(%f) = %f | " i (calculateFunc func i)
                //try
                //    printfn "y'(%f) = %f" i (derivative func i)
                //with
                //    | NotExist -> printfn "Not exist"
        with
        | _ -> printfn "Incorrect input"

        l.Stop()

        printfn "Time: %.3f s" (float l.ElapsedMilliseconds * 1e-3)

Console.ReadLine() |> ignore

(*
    pre-processing
    1. -1 => (-1)
    2. |x| => (|x|)
    3. 1 + 1 => 1+1
*)