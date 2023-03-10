module Bench

open BenchmarkDotNet.Attributes
open Node
open Derivative

[<MemoryDiagnoser>]
type NodeTest() = class
    let expr = "((-1)*sin(2*x)/cos(1/ln(x))+(x+2)*(x+3)*(x^3-1))*exp(sin(x))"
    let node = convertToFunc expr

    //[<Benchmark>]
    //member this.Convert() =
    //    convertToFunc (expr)
    let isDivOrMul1 (node: Node) =
        node.Operation = '*' || node.Operation = '/'

    let isDivOrMul2 (node: char) =
        node = '*' || node = '/'

    [<Benchmark>]
    member this.Node() =
        isDivOrMul1 node

    [<Benchmark>]
    member this.Char() =
        isDivOrMul2 node.Operation

    //[<Benchmark>]
    //member this.Derativate() =
    //    derivativeFunc node 'x'

    //[<Benchmark>]
    //member this.New() =
    //    let sub = line[..3]
    //    let result = Array.tryFind (fun (x: string) -> x = sub) funcs

    //    ()

    //[<Benchmark>]
    //member this.Old() =
    //    let result = Array.tryFind (fun (x: string) -> line.StartsWith(x)) funcs

    //    ()

    //[<Benchmark>]
    //member this.Untuple() =
    //     let (a, b) = an()
    //     a + b
    
    //[<Benchmark>]
    //member this.Tuple() =
    //     let a = fst (an())
    //     let b = snd (an())

    //     a + b

    //[<Benchmark>]
    //member this.TupleHard() =
    //     let tuple = an()
    //     let a = fst tuple
    //     let b = snd tuple

    //     a + b
end
        

