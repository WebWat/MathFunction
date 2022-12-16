open System
open System.Diagnostics
open Function
open Limit

while true do
    let l = new Stopwatch()

    printf "\nInput function: "

    let text = Console.ReadLine()

    l.Start()

    try
        let func = convertToFunc text
        printfn "String result: %s" (func.ToString())

        for i in -5.0..0.5..5.0 do
            printf "y(%f) = %f | " i (calculateFunc func i)
            try
                printfn "y'(%f) = %f" i (derivative func i)
            with
                | NotExist -> printfn "Not exist"
    with
    | _ -> printfn "Incorrect input"

    l.Stop()

    printfn "Total ms: %f" (float l.ElapsedMilliseconds * 1e-3)

Console.ReadLine() |> ignore

(*
    pre-processing
    1. -1 => (-1)
    2. |x| => (|x|)
    3. 1 + 1 => 1+1
*)