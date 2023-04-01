namespace CompMath

open Node
open Parser
open Derivative

type public FunctionMult(node: Node) =
   
    new (line: string) = FunctionMult(convertToFunc line)
        
    member _.Calc(args: Map<char, float>) = calculateFunc node args

    member _.PartialDerivative(arg: char) = FunctionMult (derivativeFunc node arg)

    member _.ArgsCount() : char array =
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
             
            

