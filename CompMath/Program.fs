module Test

open System
open System.Diagnostics
open System.Globalization


//printfn "%A" (derivative (fun x -> Math.Pow(x, x) * tan x * 1./log(x)) [|1..5|])

type Node = 
    {
    Value: Option<float>
    Operation: string
    Left: Option<Node>
    Right: Option<Node>
    }
    override this.ToString() =
        let getData (value: Option<'a>) =
            if value.IsSome then "Some(" + value.Value.ToString() + ")"
            else "None"

        $"{{Value = {getData(this.Value)}; Operation = \"{this.Operation}\"; Left = {getData(this.Left)}; Right = {getData(this.Right)};}}"

let isNumber (number) =
    let mutable temp = 0.0
    // Parse with a dot 
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

let getBrackets (data: char[]) =
    let lbr = Array.IndexOf(data, '(')

    let rec find total current be =
        if total = 0 && be then
            current - 1
        elif data[current] = '(' then find (total + 1) (current + 1) true
        elif data[current] = ')' then find (total - 1) (current + 1) true
        else find total (current + 1) be

    if lbr = -1 then (-1, -1)
    else (lbr, find 0 0 false)

let checkValueInBrackets (left: int) (right: int) (symbol: char) (line: char[])  =
    let rec ifInCurrentBracket (current: int) : int =
        if (left, right) = (-1, -1) then 
            Array.IndexOf(line, symbol)
        elif current = line.Length then 
            -1
        else
            if line[current] = symbol && (current > right || current < left) then
                current
            else
                ifInCurrentBracket (current + 1)
    
    let rec checkRightSide (line: char[]) (rLast: int) (rCurrent: int) ind =
        let aff = line[rLast + 2..]
        let (l, r) = getBrackets aff
        
        let offset = ind - rCurrent - 2
        
        if l = -1 || r = -1 || l > offset then
            ind
        elif offset < r then
            -1
        else
            checkRightSide aff r (r + rCurrent + 2) ind
    
    let index = ifInCurrentBracket 0

    if right <> -1 && index > right + 1 then
        checkRightSide line right right index
    else
        index

 
let rec clearBrackets (line: string) =
    let laz = lazy(getBrackets (line.ToCharArray()))

    if line[0] = '(' && (laz.Force()) = (0, line.Length - 1) then
        clearBrackets line[1..line.Length - 2]
    else
        line 

let rec breakLine (symbol: char) (line: string) =
    let charArray = line.ToCharArray()

    let (lbracket, rbracket) = getBrackets charArray

    let index = checkValueInBrackets lbracket rbracket symbol charArray

    // Right: multiply & divide
    if index <> -1 && lbracket <> -1 && lbracket < index && symbol <> '+' && symbol <> '-' then
        let b = charArray[rbracket..rbracket + 1] 

        let leftS = Array.IndexOf(b, symbol)
        
        if leftS <> -1 then 
            (line[1..rbracket - 1], line[rbracket + 2..])
        else
            ("-", "-")
    // Left: all others
    elif index <> -1 then
        (line[..index - 1], line[index + 1..])
    else
        ("-", "-")

let convert2func (line: string) : Node =
    let operations = [|'+';'-';'*';'/';'^'|]

    let rec searchOperation (arg: string * string) (line: string) (i: int) =
        match arg with 
        | ("-", "-") -> searchOperation (breakLine (operations[i + 1]) line) (line) (i + 1)
        | (val1, val2) -> (val1, val2, i)

    let rec convert line =
        let removed = clearBrackets line

        match removed with 
        | "x" ->  { Value = None;          Operation = "x"; Right = None; Left = None }
        | "pi" -> { Value = Some(Math.PI); Operation = "x"; Right = None; Left = None }
        | "e" ->  { Value = Some(Math.E);  Operation = "x"; Right = None; Left = None }
        | val1 when isNumber(val1)     -> { Value = Some(float val1); Operation = ""; Right = None; Left = None } //!
        | val1 when val1[0..1] = "ln"  -> { Value = None; Operation = "ln";  Left = None; Right = Some(convert(val1[3..val1.Length - 2])) }
        //| val1 when val1[0..1] = "lg"  -> { Value = None; Operation = "lg";  Left = None; Right = Some(convert(val1[3..val1.Length - 2])) }
        //| val1 when val1[0..2] = "log" -> { Value = None; Operation = "log" + string val1[3]; Left = None; Right = Some(convert(val1[5..val1.Length - 2])) }
        //| val1 when val1[0..2] = "sin" -> { Value = None; Operation = "sin"; Left = None; Right = Some(convert(val1[4..val1.Length - 2])) }
        //| val1 when val1[0..2] = "cos" -> { Value = None; Operation = "cos"; Left = None; Right = Some(convert(val1[4..val1.Length - 2])) }
        //| val1 when val1[0..1] = "tg"  -> { Value = None; Operation =  "tg";  Left = None; Right = Some(convert(val1[3..val1.Length - 2])) }
        | _ ->  let (lft, rght, i) = searchOperation (breakLine operations[0] removed) removed 0
                printfn "%s <%c> %s" lft operations[i] rght
                { Value = None; Operation = string operations[i]; Right = Some(convert(rght)); Left = Some(convert(lft)) }

    convert line
            

let rec calculateFunc (x: float) (node: Node) : float =
    if node.Value.IsSome then 
        node.Value.Value
    else
        match node.Operation with
        | "x" -> x
        | "ln"  -> Math.Log(calculateFunc x node.Right.Value)
        //| "lg"  -> Math.Log10(calculateFunc x node.Right.Value)
        //| "sin" -> Math.Sin(calculateFunc x node.Right.Value)
        //| "cos" -> Math.Cos(calculateFunc x node.Right.Value)
        //| "tg"  -> Math.Tan(calculateFunc x node.Right.Value)
        | "^"   -> Math.Pow(calculateFunc x node.Left.Value, calculateFunc x node.Right.Value)
        | "*"   -> calculateFunc x node.Left.Value * calculateFunc x node.Right.Value
        | "/"   -> calculateFunc x node.Left.Value / calculateFunc x node.Right.Value
        | "+"   -> calculateFunc x node.Left.Value + calculateFunc x node.Right.Value
        | "-"   -> calculateFunc x node.Left.Value - calculateFunc x node.Right.Value
        | val1 when val1[0..2] = "log" -> Math.Log(calculateFunc x node.Right.Value, float (string val1[3])) // only 1-9!!!
        | _ -> failwith "Not available"

let l = new Stopwatch()

l.Start()
let func = convert2func "cos(x)^2"

for i = 0 to 5 do
    printfn "y( %d ) = %f" i (calculateFunc (float i) func)

l.Stop()

printfn "Total ms: %f" (float l.ElapsedMilliseconds * 1e-3)
//printfn "Total ms: %f" (Math.Log(3, 3))

//printfn "Result: %d" (calculate (convert2func "(2+2)*2+(2+2)*2+(2+2*2)"))

Console.ReadLine() |> ignore
