namespace CompMath

open Node

[<AbstractClass>]
type SingleArgFunction(node: Node) =
    abstract Calc: float -> float

    abstract Derivative: unit -> SingleArgFunction

    member _.ToMathJax() = node.ToMathJax()

    override _.ToString() = node.ToString()
