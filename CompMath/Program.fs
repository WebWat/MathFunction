module Test

open System


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

let getBrackets (data: string) =
    let lbr = Array.IndexOf(data.ToCharArray(), '(')

    let rec find total current be =
        if total = 0 && be then
            current - 1
        elif data[current] = '(' then find (total + 1) (current + 1) true
        elif data[current] = ')' then find (total - 1) (current + 1) true
        else find total (current + 1) be

    if lbr = -1 then (-1, -1)
    else (lbr, find 0 0 false)

let checkPos (left: int) (right: int) (symbol: char) (line: string) (current: int)  =
    let rec op (current: int) : int =
        if (left, right) = (-1, -1) then 
            Array.IndexOf(line.ToCharArray(), symbol)
        elif current = line.Length then 
            -1
        else
            if line[current] = symbol && (current > right || current < left) then
                current
            else
                op (current + 1)

    let index = op 0

    if index > right + 1 then
        let aff = line[right + 1..]
        let (l, r) = getBrackets (aff)    
        let offset = index - right - 1

        if l < offset && offset < r then
            -1
        else
            index 
    else
        index


let breakLine (symbol: char) (line: string) =
    let clearBracket (line: string) =
        let (l, r) = getBrackets line
        if line[0] = '(' && 0 = l && line.Length - 1 = r then
            line[1..line.Length - 2]
        else
            line 

    let data = line
    let charArray = data.ToCharArray()

    let (lbracket, rbracket) = getBrackets data

    let index = checkPos lbracket rbracket symbol line 0

    // Right multiply / divide
    if index <> -1 && lbracket <> -1 && lbracket < index && symbol <> '+' && symbol <> '-' then
        let b = charArray[rbracket..rbracket + 1] 

        let leftS = Array.IndexOf(b, symbol)
        
        if leftS <> -1 then 
            (data[1..rbracket - 1], clearBracket data[rbracket + 2..])
        else
            ("-", "-")
    // Left operation
    elif index <> -1 then
        (data[..index - 1], clearBracket data[index + 1..])
    else
        ("-", "-")

let convert2func (line: string) : Node =
    let op = [|'+';'-';'*';'/';|]

    let rec searchOperation (arg: string * string) (line: string) (i: int) =
        match arg with 
        | ("-", "-") -> searchOperation (breakLine (op[i + 1]) line) (line) (i + 1)
        | (val1, val2) -> (val1, val2, i)

    let rec convert line =
        if isNumber(line) then 
            { Value = Some(int line); Operation = ""; Right = None; Left = None }
        else
            let (lft, rght, i) = searchOperation (breakLine op[0] line) line 0
            printfn "%s <-> %s" lft rght
            { Value = None; Operation = string op[i]; Right = Some(convert(rght)); Left = Some(convert(lft)) }

    convert line
            

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


printfn "Result: %d" (calculate (convert2func "(2+2)*(2+3)*(2+4)"))

Console.ReadLine() |> ignore
