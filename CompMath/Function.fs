module Function

open System
open System.Globalization
open System.Text.RegularExpressions

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

        $"{{Value = {getData(this.Value)};Operation = \"{this.Operation}\";Left = {getData(this.Left)};Right = {getData(this.Right)};}}"

    member private this.IsComplexOperation (op: string) =
        Array.contains op [|"ln"; "lg"; "sin"; "cos"; "tg"; "ctg"; "sqrt"; "^"; "+"; "-"; "|"|]

    member private this.Func2String (op: string) =
        match this.Operation with
        | "" -> string this.Value.Value
        | "x" -> "x"
        | "+" -> if this.IsComplexOperation op then 
                    $"""{this.Left.Value.Func2String "+"} + {this.Right.Value.Func2String "+"}""" 
                 else
                    $"""({this.Left.Value.Func2String "+"} + {this.Right.Value.Func2String "+"})"""
        | "-" -> if this.IsComplexOperation op then 
                    $"""{this.Left.Value.Func2String "-"} - {this.Right.Value.Func2String "-"}""" 
                 else
                    $"""({this.Left.Value.Func2String "-"} - {this.Right.Value.Func2String "-"})"""
        | "*" -> if this.IsComplexOperation op || op = "*" then 
                    $"""{this.Left.Value.Func2String "*"}*{this.Right.Value.Func2String "*"}""" 
                 else
                    $"""({this.Left.Value.Func2String "*"}*{this.Right.Value.Func2String "*"})"""
        | "/" -> if this.IsComplexOperation op then 
                    $"""{this.Left.Value.Func2String "/"}/{this.Right.Value.Func2String "/"}""" 
                 else
                    $"""({this.Left.Value.Func2String "/"}/{this.Right.Value.Func2String "/"})"""
        | "^"    -> $"""({this.Left.Value.Func2String "^"})^({this.Right.Value.Func2String "^"})"""
        | "|"    -> $"""|{this.Right.Value.Func2String "|"}|"""
        | "ln"   -> $"""ln({this.Right.Value.Func2String "ln"})"""
        | "lg"   -> $"""lg({this.Right.Value.Func2String "lg"})"""
        | "sin"  -> $"""sin({this.Right.Value.Func2String "sin"})"""
        | "cos"  -> $"""cos({this.Right.Value.Func2String "cos"})"""
        | "sqrt" -> $"""sqrt({this.Right.Value.Func2String "sqrt"})"""
        | "tg"   -> $"""tg({this.Right.Value.Func2String "tg"})"""
        | "ctg"  -> $"""ctg({this.Right.Value.Func2String "ctg"})"""
        | _ -> failwith "Not working"

    override this.ToString() =
        let result = this.Func2String(this.Operation)
        result

let isNumber (number) =
    let mutable temp = 0.0
    // Parse with a dot
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

let getBrackets (line: char[]) =
    // refactoring!!
    let getClose leftBracket leftModule =
        if leftModule = -1 && leftBracket <> -1 || 
           (leftBracket <> -1 && leftModule <> -1 && leftBracket < leftModule) then 
           (leftBracket, '(')
        elif leftBracket = -1 && leftModule <> -1 || 
             (leftModule <> -1 && leftBracket <> -1 && leftModule < leftBracket) then 
             (leftModule, '|')
        else (-1, '-')

    let rec findBracket total current =
        if total = 0 then current - 1
        elif line[current] = '(' then findBracket (total + 1) (current + 1)
        elif line[current] = ')' then findBracket (total - 1) (current + 1)
        else findBracket total (current + 1)

    let rec findModule total current notClosed =
        if total = 0 then
            current - 1
        elif line[current] = '|' then
            if notClosed && Regex.IsMatch(string line[current - 1], @"\W") then 
                findModule (total + 1) (current + 1) true
            else
                findModule (total - 1) (current + 1) false
        else findModule total (current + 1) true

    let (left, operation) = getClose (Array.IndexOf(line, '(')) (Array.IndexOf(line, '|'))

    match operation with
    | '(' -> (left, findBracket 1 (left + 1))
    | '|' -> (left, findModule 1 (left + 1) true)
    | _ -> (-1, -1)

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

(*
    pre-processing
    1. -1 => (-1)
    2. |x| => (|x|)
    3. 1 + 1 => 1+1
*)
let preProcessing (line: string) : string =
    let matchesNegative = Regex.Matches(line, @"\D-[0-9]*(\.[0-9]*)?")

    let rec addBrackets2NegativeNum (line: string) (i: int) (matches: MatchCollection) = 
        if i = matches.Count then
            line
        else
            let _match = matches[i].Value[1..matches[i].Value.Length - 1]
            addBrackets2NegativeNum (line.Replace(_match, "(" + _match + ")")) (i + 1) matches

    (addBrackets2NegativeNum line 0 matchesNegative).Replace(" ", "")

let convertToFunc (line: string) : Node =
    let operations = [|'+';'-';'*';'/';'^'|]

    //  Looping through all operations
    let rec searchOperation (arg: string * string) (line: string) (brackets: int * int) (i: int) =
        match arg with 
        | ("-", "-") -> searchOperation (breakLine brackets (operations[i + 1]) line) line brackets (i + 1)
        | (val1, val2) -> (val1, val2, i)

    let rec convert (line: string) =
        let charArray = line.ToCharArray()
        printfn "%s" line
        // Removing extra brackets
        let (removed, (l, r)) = clearBrackets charArray

        match removed with 
        | "x" ->  { Value = None;          Operation = "x"; Left = None; Right = None; }
        | "pi" -> { Value = Some(Math.PI); Operation = ""; Left = None; Right = None; }
        | "e" ->  { Value = Some(Math.E);  Operation = ""; Left = None; Right = None; }
        | val1 when isNumber(val1) -> { Value = Some(float val1); Operation = ""; Right = None; Left = None }
        | val1 when r = val1.Length - 1 -> 
                match val1 with
                | op when op.StartsWith("|")  ->  { Value = None; Operation = "|";    Right = Some(convert(op[1..val1.Length - 2])); Left = None; }
                | op when op.StartsWith("ln")  -> { Value = None; Operation = "ln";   Right = Some(convert(op[3..r - 1])); Left = None; }
                | op when op.StartsWith("lg")  -> { Value = None; Operation = "lg";   Right = Some(convert(op[3..r - 1])); Left = None; }        
                | op when op.StartsWith("tg")  -> { Value = None; Operation = "tg";   Right = Some(convert(op[3..r - 1])); Left = None; }
                | op when op.StartsWith("sin") -> { Value = None; Operation = "sin";  Right = Some(convert(op[4..r - 1])); Left = None; }
                | op when op.StartsWith("cos") -> { Value = None; Operation = "cos";  Right = Some(convert(op[4..r - 1])); Left = None; }
                | op when op.StartsWith("ctg") -> { Value = None; Operation = "ctg";  Right = Some(convert(op[4..r - 1])); Left = None; }
                | op when op.StartsWith("sqrt")-> { Value = None; Operation = "sqrt"; Right = Some(convert(op[5..r - 1])); Left = None; }
                | _ -> let (lft, rght, i) = searchOperation (breakLine (l, r) operations[0] removed) removed (l, r) 0
                       { Value = None; Operation = string operations[i]; Left = Some(convert(lft)); Right = Some(convert(rght)) }
        | _ -> let (lft, rght, i) = searchOperation (breakLine (l, r) operations[0] removed) removed (l, r) 0
               { Value = None; Operation = string operations[i]; Left = Some(convert(lft)); Right = Some(convert(rght)) }

    convert line
            

let rec calculateFunc (node: Node) (x: float) : float =
    if node.Value.IsSome then 
        node.Value.Value
    else
        match node.Operation with
        | "x" -> x
        | "ln"   -> Math.Log(calculateFunc node.Right.Value x)
        | "lg"   -> Math.Log10(calculateFunc node.Right.Value x)
        | "sin"  -> Math.Sin(calculateFunc node.Right.Value x)
        | "cos"  -> Math.Cos(calculateFunc node.Right.Value x)
        | "sqrt" -> Math.Sqrt(calculateFunc node.Right.Value x)
        | "tg"   -> Math.Tan(calculateFunc node.Right.Value x)
        | "ctg"  -> 1. / Math.Tan(calculateFunc node.Right.Value x)
        | "|"    -> Math.Abs(calculateFunc node.Right.Value x)
        | "^"    -> Math.Pow(calculateFunc node.Left.Value x, calculateFunc node.Right.Value x)
        | "*"    -> calculateFunc node.Left.Value x * calculateFunc node.Right.Value x
        | "/"    -> calculateFunc node.Left.Value x / calculateFunc node.Right.Value x
        | "+"    -> calculateFunc node.Left.Value x + calculateFunc node.Right.Value x
        | "-"    -> calculateFunc node.Left.Value x - calculateFunc node.Right.Value x
        | _ -> failwith "Not available"