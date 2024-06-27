namespace FunctionParser

open Node

[<AbstractClass>]
type public SingleArgFunction(node: Node) =

    abstract Calc: float -> float

    abstract Derivative: unit -> SingleArgFunction

    member _.ToMathJax() = node.ToMathJax()

    override _.ToString() = node.ToString()
