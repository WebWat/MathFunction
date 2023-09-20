namespace MathFunction

open Node
open Parser
open Derivative

type public MultivariableFunction(node: Node) =
   
    new (line: string) = MultivariableFunction(convertToFunction line)
        
    member _.Calc(args: Map<char, float>) = calculateFunction node args

    member _.PartialDerivative(arg: char) = MultivariableFunction (functionDerivative node arg)

    member _.GetArgs() : char array =
        let rec find (node: Node) (values: char array) : char array =
            match (node.Left.IsSome, node.Right.IsSome) with
            | (true, true) -> 
                if isArg node.Left.Value && isArg node.Right.Value then
                    Array.append values [|node.Left.Value.Operation|] |> Array.append [|node.Right.Value.Operation|]
                elif isArg node.Left.Value then
                    Array.append (find node.Right.Value values) [|node.Left.Value.Operation|]
                elif isArg node.Right.Value then
                    Array.append (find node.Left.Value values) [|node.Right.Value.Operation|]
                else
                    Array.append (find node.Left.Value values) (find node.Right.Value values)
            | (_, true) -> 
                if isArg node.Right.Value then
                    Array.append values [|node.Right.Value.Operation|]
                else
                    find node.Right.Value values
            | _ -> [||]

        find node [||] |> Array.sort |> Array.distinct

    override _.ToString() = node.ToString()
             
            

