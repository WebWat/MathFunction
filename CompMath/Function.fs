namespace CompMath

open Node

type public Function(line: string) =
    let node: Node = convertToFunc line

    member _.Calc (x: float) = calculateFunc node x 

    override _.ToString() = node.ToString()

