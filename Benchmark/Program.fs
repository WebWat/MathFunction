open System
open Bench
open BenchmarkDotNet.Running

BenchmarkRunner.Run<NodeTest>() |> ignore

Console.ReadLine() |> ignore
