module Bench

open Parser
open Derivative
open BenchmarkDotNet.Attributes

[<MemoryDiagnoser>]
type NodeTest() = class
    let expr = "((-1)*sin(2*x)/cos(1/ln(x))+(x+2)*(x+3)*(x^3-1))*exp(sin(x))"
    let node = convertToFunction expr

    [<Benchmark>]
    member this.Convert() =
        convertToFunction (expr)

    [<Benchmark>]
    member this.Derativate() =
        functionDerivative node 'x'
end
        

