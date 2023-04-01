namespace CompMath

open Node
open Parser
open Derivative

type public FunctionX(node: Node) =
    inherit SingleArgFunction(node)

    new (line: string) = FunctionX(convertToFunc line)
        
    override _.Calc(x: float) = calculateFunc node (Map [('x', x)])

    override _.Derivative() = FunctionX (derivativeFunc node 'x')

    override _.ToString() = node.ToString()
