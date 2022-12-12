open System
open Main


//printfn "%A" (derivative (fun x -> Math.Pow(x, x) * tan x * 1./log(x)) [|1..5|])

type Node = 
    {
    Value: Option<int>
    Operation: string
    Left: Option<Node>
    Right: Option<Node>
    }
    override this.ToString() =
        let getData (value: Option<'a>) =
            if value.IsSome then value.Value.ToString()
            else "None"

        $"Value: [{getData(this.Value)}]; Operation: [{this.Operation}]; Left: [{getData(this.Left)}]; Right: [{getData(this.Right)}]"

let isNumber (number) =
    let mutable temp = 0
    Int32.TryParse(number.ToString(), &temp)

let IsOne (symbol: char) (data: string) = 
    let result = data.Split(symbol)
    result.Length = 1

let rec convert (line: string) : Node =
    if isNumber(line) then 
        { Value = Some(int line); Operation = ""; Right = None; Left = None }
    else
        if not (IsOne '*' line) then
            let arr = line.Split('*');
            let (lft, rght) = (arr[0], arr[1])
            { Value = None; Operation = "*"; Right = Some(convert(lft)); Left = Some(convert(rght))}
        elif not (IsOne '/' line) then
            let arr = line.Split('/');
            let (lft, rght) = (arr[0], arr[1])
            { Value = None; Operation = "/"; Right = Some(convert(lft)); Left = Some(convert(rght))}
        elif not (IsOne '+' line) then
            let arr = line.Split('+');
            let (lft, rght) = (arr[0], arr[1])
            { Value = None; Operation = "+"; Right = Some(convert(lft)); Left = Some(convert(rght))}
        else
            let arr = line.Split('-');
            let (lft, rght) = (arr[0], arr[1])
            { Value = None; Operation = "-"; Right = Some(convert(lft)); Left = Some(convert(rght))}

let rec calculate (node: Node) : int =
    if node.Value.IsSome then 
        node.Value.Value
    else
        match node.Operation with
        | "*" -> calculate node.Left.Value * calculate node.Right.Value
        | "/" -> calculate node.Left.Value / calculate node.Right.Value
        | "+" -> calculate node.Left.Value + calculate node.Right.Value
        | "-" -> calculate node.Left.Value - calculate node.Right.Value
        | _ -> failwith "Not working"


let line = "2+2*2"
let node = convert line
let result = calculate node

printfn "%s\nResult: %d" (node.ToString()) result

Console.ReadLine() |> ignore
