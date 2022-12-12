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

let findRightBracket (data: string) =
    let rec find total current be =
        if total = 0 && be then
            current - 1
        elif data[current] = '(' then find (total + 1) (current + 1) true
        elif data[current] = ')' then find (total - 1) (current + 1) true
        else find total (current + 1) be

    find 0 0 false

let getLines (symbol: char) (line: string) =
    let clearBracket (line: string) = 
        if line[0] = '(' && line[line.Length - 1] = ')' then line[1..line.Length - 2]
        else line

    let data = line
    let charArray = data.ToCharArray()
    let lbracket = Array.IndexOf(charArray, '(')
    let index = Array.IndexOf(charArray, symbol)
    let rbracket = findRightBracket data

    if lbracket <> -1 && rbracket = data.Length - 1 then
        let a = charArray[..lbracket] 
        let b = charArray[rbracket..rbracket + 1] 

        let leftS = Array.IndexOf(b, symbol)
        let rightS = Array.IndexOf(a, symbol)
        
        if leftS <> -1 then 
            (data[1..rbracket - 1], clearBracket data[leftS + 1..])
        elif rightS <> -1 then 
            (clearBracket data[0..rightS - 1], data[lbracket + 1..data.Length - 2])
        else
            ("-", "-")
    elif index <> -1 then
        (data[..index - 1], data[index + 1..])
    else
        ("-", "-")

let op = [|'+';'-';'*';'/';|]

let rec check (arg: string * string) (line: string) (i: int) : (string * string * int) =
    match arg with 
    | (val1, val2) when val1 <> "-" && val2 <> "-" -> (val1, val2, i)
    | _ -> check (getLines (op[i]) line) (line) (i + 1)

let rec convert (line: string) : Node =
    if isNumber(line) then 
        { Value = Some(int line); Operation = ""; Right = None; Left = None }
    else
        let (lft, rght, i) = check (getLines op[0] line) line 0
        printfn "%s <-> %s" lft rght
        { Value = None; Operation = string op[i]; Right = Some(convert(rght)); Left = Some(convert(lft)) }
            

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


let line = "(2+2)*2+2"
let node = convert line
let result = calculate node

printfn "%s\nResult: %d" (node.ToString()) result

Console.ReadLine() |> ignore
