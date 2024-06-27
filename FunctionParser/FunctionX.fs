namespace FunctionParser

open Node
open Parser
open Derivative

type public FunctionX(node: Node) =
    inherit SingleArgFunction(node)

    new (line: string) = FunctionX(convertToFunction line)
        
    override _.Calc(x: float) = calculateFunction node (Map [('x', x)])

    override _.Derivative() = FunctionX (functionDerivative node 'x')

    override _.ToString() = node.ToString()
