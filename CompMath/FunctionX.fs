namespace CompMath

open Node

type public FunctionX(line: string) =
    let node: Node = convertToFunc line

    member _.Calc(x: float) = calculateFunc node (Map [('x', x)])

    override _.ToString() = node.ToString()
