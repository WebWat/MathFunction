module Test

open System
open System.Diagnostics
open System.Globalization
open System.Text.RegularExpressions


//printfn "%A" (derivative (fun x -> Math.Pow(x, x) * tan x * 1./log(x)) [|1..5|])

type Node = 
    {
    Value: Option<float>
    Operation: string
    Left: Option<Node>
    Right: Option<Node>
    }
    member this.ToRecord() =
        let getData (value: Option<'a>) =
            if value.IsSome then "Some(" + value.Value.ToString() + ")"
            else "None"

        $"{{Value = {getData(this.Value)}; Operation = \"{this.Operation}\"; Left = {getData(this.Left)}; Right = {getData(this.Right)};}}"

    override this.ToString() =
        let getData (value: Option<'a>) =
            if value.IsSome then "Some(" + value.Value.ToString() + ")"
            else "None"

        $"{{Value = {getData(this.Value)}; Operation = \"{this.Operation}\"; Left = {getData(this.Left)}; Right = {getData(this.Right)};}}"

let isNumber (number) =
    let mutable temp = 0.0
    // Parse with a dot
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

let getBrackets (line: char[]) =
    let leftBracket = Array.IndexOf(line, '(')

    let rec find total current be =
        if total = 0 && be then
            current - 1
        elif line[current] = '(' then find (total + 1) (current + 1) true
        elif line[current] = ')' then find (total - 1) (current + 1) true
        else find total (current + 1) be

    if leftBracket = -1 then (-1, -1)
    else (leftBracket, find 0 0 false)

let checkValueInBrackets (left: int) (right: int) (symbol: char) (line: char[])  =
    let rec lookOutsideBracket (current: int) : int =
        if (left, right) = (-1, -1) then 
            Array.IndexOf(line, symbol)
        elif current = line.Length then 
            -1
        else
            if line[current] = symbol then
                current
            // Skip inner
            elif current = left then
                lookOutsideBracket (current + right - left + 1)
            else
                lookOutsideBracket (current + 1)
    
    let rec checkRightSide (line: char[]) (rLast: int) (rCurrent: int) index =
        let subLine = line[rLast + 2..]
        let (left, right) = getBrackets subLine
        
        let offset = index - rCurrent - 2
        
        if left = -1 || right = -1 || left > offset then
            index
        elif offset < right then
            -1
        else
            checkRightSide subLine right (right + rCurrent + 2) index
    
    let index = lookOutsideBracket 0

    // If found beyond expression
    if right <> -1 && index > right + 1 then
        checkRightSide line right right index
    else
        index

 
let rec clearBrackets (line: char[]) =
    let args = getBrackets line

    if line[0] = '(' && args = (0, line.Length - 1) then
        clearBrackets line[1..line.Length - 2]
    else
        (new string(line), args)

// Splitting a string by operations
let rec breakLine (brackets: int * int) (symbol: char) (line: string) =
    let charArray = line.ToCharArray()

    let (lbracket, rbracket) = brackets

    let index = checkValueInBrackets lbracket rbracket symbol charArray

    let comp = lazy(Array.IndexOf(charArray[rbracket..rbracket + 1], symbol))

    // If symbol not found
    if index = -1 then
        ("-", "-")
    // If multiply, divide and pow (right side only)
    elif lbracket <> -1 && lbracket < index && symbol <> '+' && symbol <> '-' && comp.Force() <> -1 then
        (line[0..rbracket], line[rbracket + 2..])
    // If all other operations (left side only)
    else
        (line[..index - 1], line[index + 1..])

let convert2func (line: string) : Node =
    let operations = [|'+';'-';'*';'/';'^'|]

    //  Looping through all operations
    let rec searchOperation (arg: string * string) (line: string) (brackets: int * int) (i: int) =
        match arg with 
        | ("-", "-") -> searchOperation (breakLine brackets (operations[i + 1]) line) line brackets (i + 1)
        | (val1, val2) -> (val1, val2, i)

    let rec convert (line: string) =
        let charArray = line.ToCharArray()

        // Removing extra brackets
        let (removed, (l, r)) = clearBrackets charArray

        match removed with 
        | "x" ->  { Value = None;          Operation = "x"; Left = None; Right = None; }
        | "pi" -> { Value = Some(Math.PI); Operation = "x"; Left = None; Right = None; }
        | "e" ->  { Value = Some(Math.E);  Operation = "x"; Left = None; Right = None; }
        | val1 when isNumber(val1)     -> { Value = Some(float val1); Operation = ""; Right = None; Left = None }
        | val1 when val1.StartsWith("|") && val1[val1.Length - 1] = '|' -> { Value = None; Operation = "|"; Right = Some(convert(val1[1..val1.Length - 2])); Left = None; } // Only 1 deep
        | val1 when r = val1.Length - 1 && val1.StartsWith("ln")  -> { Value = None; Operation = "ln";   Right = Some(convert(val1[3..r - 1])); Left = None; }
        | val1 when r = val1.Length - 1 && val1.StartsWith("lg")  -> { Value = None; Operation = "lg";   Right = Some(convert(val1[3..r - 1])); Left = None; }        
        | val1 when r = val1.Length - 1 && val1.StartsWith("tg")  -> { Value = None; Operation = "tg";   Right = Some(convert(val1[3..r - 1])); Left = None; }
        | val1 when r = val1.Length - 1 && val1.StartsWith("sin") -> { Value = None; Operation = "sin";  Right = Some(convert(val1[4..r - 1])); Left = None; }
        | val1 when r = val1.Length - 1 && val1.StartsWith("cos") -> { Value = None; Operation = "cos";  Right = Some(convert(val1[4..r - 1])); Left = None; }
        | val1 when r = val1.Length - 1 && val1.StartsWith("ctg")  -> { Value = None; Operation = "ctg"; Right = Some(convert(val1[4..r - 1])); Left = None; }
        | val1 when r = val1.Length - 1 && val1.StartsWith("sqrt")-> { Value = None; Operation = "sqrt"; Right = Some(convert(val1[5..r - 1])); Left = None; }
        | _ ->  let (lft, rght, i) = searchOperation (breakLine (l, r) operations[0] removed) removed (l, r) 0
                { Value = None; Operation = string operations[i]; Left = Some(convert(lft)); Right = Some(convert(rght)) }

    convert line
            

let rec calculateFunc (x: float) (node: Node) : float =
    if node.Value.IsSome then 
        node.Value.Value
    else
        match node.Operation with
        | "x" -> x
        | "ln"   -> Math.Log(calculateFunc x node.Right.Value)
        | "lg"   -> Math.Log10(calculateFunc x node.Right.Value)
        | "sin"  -> Math.Sin(calculateFunc x node.Right.Value)
        | "cos"  -> Math.Cos(calculateFunc x node.Right.Value)
        | "sqrt" -> Math.Sqrt(calculateFunc x node.Right.Value)
        | "tg"   -> Math.Tan(calculateFunc x node.Right.Value)
        | "ctg"  -> 1. / Math.Tan(calculateFunc x node.Right.Value)
        | "|"    -> Math.Abs(calculateFunc x node.Right.Value)
        | "^"    -> Math.Pow(calculateFunc x node.Left.Value, calculateFunc x node.Right.Value)
        | "*"    -> calculateFunc x node.Left.Value * calculateFunc x node.Right.Value
        | "/"    -> calculateFunc x node.Left.Value / calculateFunc x node.Right.Value
        | "+"    -> calculateFunc x node.Left.Value + calculateFunc x node.Right.Value
        | "-"    -> calculateFunc x node.Left.Value - calculateFunc x node.Right.Value
        | _ -> failwith "Not available"

while true do
    let l = new Stopwatch()

    //printf "\nInput function: "

    let text = "(-1)*(sin(2*x)-2*sin(pi))/(x*ln(cos(5*x)))"

    l.Start()

    try
        let func = convert2func text
        printfn "%s" (func.ToRecord())
        Console.ReadLine() |> ignore
        for i in -3.0..0.5..3.0 do
            printfn "y( %f ) = %f" i (calculateFunc i func)
    with
    | _ -> printfn "Incorrect input!"

    l.Stop()

    printfn "Total ms: %f" (float l.ElapsedMilliseconds * 1e-3)

Console.ReadLine() |> ignore

(*
    pre-processing
    1. -1 => (-1)
    2. |x| => (|x|)
    3. 1 + 1 => 1+1
*)