namespace CompMath

open Node
open Derivative

type public FunctionX(node: Node) =
    new (line: string) = FunctionX(convertToFunc line)
        
    member _.Calc(x: float) = calculateFunc node (Map [('x', x)])

    member _.Deravative() = FunctionX (derivativeFunc node 'x')

    override _.ToString() = node.ToString()
