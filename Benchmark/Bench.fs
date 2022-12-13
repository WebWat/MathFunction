module Bench

open BenchmarkDotNet.Attributes
open Test

[<MemoryDiagnoser>]
type NodeTest() = class

    let node: Node = {Value = None; Operation = "*"; Left = Some({Value = None; Operation = "+"; Left = Some({Value = Some(222); Operation = ""; Left = None; Right = None;}); Right = Some({Value = Some(2); Operation = ""; Left = None; Right = None;});}); Right = Some({Value = None; Operation = "/"; Left = Some({Value = None; Operation = "-"; Left = Some({Value = Some(2); Operation = ""; Left = None; Right = None;}); Right = Some({Value = Some(2); Operation = ""; Left = None; Right = None;});}); Right = Some({Value = None; Operation = "+"; Left = Some({Value = Some(2); Operation = ""; Left = None; Right = None;}); Right = Some({Value = Some(2); Operation = ""; Left = None; Right = None;});});});}

    member private this.Expr () = "(222+2)*(2-2)/(2+2)"

    [<Benchmark>]
    member this.Convert() =
         convert2func (this.Expr())

    //[<Benchmark>]
    member this.Calculate() =

        let result = calculateFunc 0 node
        ()
end
        

