module Bench

open BenchmarkDotNet.Attributes
open Node
open Expand

[<MemoryDiagnoser>]
type NodeTest() = class

    let node: Node = {Value = None; Operation = "*"; Left = Some({Value = Some(-1); Operation = ""; Left = None; Right = None;}); Right = Some({Value = None; Operation = "/"; Left = Some({Value = None; Operation = "-"; Left = Some({Value = None; Operation = "sin"; Left = None; Right = Some({Value = None; Operation = "*"; Left = Some({Value = Some(2); Operation = ""; Left = None; Right = None;}); Right = Some({Value = None; Operation = "x"; Left = None; Right = None;});});}); Right = Some({Value = None; Operation = "*"; Left = Some({Value = Some(2); Operation = ""; Left = None; Right = None;}); Right = Some({Value = None; Operation = "sin"; Left = None; Right = Some({Value = Some(3.141592653589793); Operation = "x"; Left = None; Right = None;});});});}); Right = Some({Value = None; Operation = "*"; Left = Some({Value = None; Operation = "x"; Left = None; Right = None;}); Right = Some({Value = None; Operation = "ln"; Left = None; Right = Some({Value = None; Operation = "cos"; Left = None; Right = Some({Value = None; Operation = "*"; Left = Some({Value = Some(5); Operation = ""; Left = None; Right = None;}); Right = Some({Value = None; Operation = "x"; Left = None; Right = None;});});});});});});}
    let node2: Node = { Value = None
                        Operation = "sin"
                        Left = None
                        Right = Some { Value = None
                                       Operation = "x"
                                       Left = None
                                       Right = None } }
    member private this.Expr () = "((-1)*sin(2*x)/cos(1/ln(x))+(x+2)*(x+3)*(x^3-1))*exp(sin(x))"//"(-1)*(sin(2*x)-2*sin(x))/(x*ln(cos(5*x)))"

    //[<Benchmark>]
    //member this.Convert() =
    //    convertToFunc (this.Expr())

    //[<Benchmark>]
    //member this.Calculate() =
    //    calculateFunc node 0.05

    [<Benchmark>]
    member this.MultiplyAll() =
         multiplyAll node2
end
        

