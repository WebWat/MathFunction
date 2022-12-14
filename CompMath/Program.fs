open System
open System.Diagnostics
open Function

while true do
    let l = new Stopwatch()

    //printf "\nInput function: "

    let text = "(-1)*(sin(2*x)-2*sin(pi))/(x*ln(cos(5*x)))"

    l.Start()

    try
        let func = convert2func text
        printfn "String result: %s" (func.ToString())
        Console.ReadLine() |> ignore
        for i in -3.0..0.5..3.0 do
            printfn "y( %f ) = %f" i (calculateFunc i func)
    with
    | _ -> printfn "Incorrect input!"

    l.Stop()

    printfn "Total ms: %f" (float l.ElapsedMilliseconds * 1e-3)

Console.ReadLine() |> ignore

(*
    pre-processing
    1. -1 => (-1)
    2. |x| => (|x|)
    3. 1 + 1 => 1+1
*)