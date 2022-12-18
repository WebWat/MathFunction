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
        Array.contains op [|"ln"; "lg"; "sin"; "cos"; "tg"; "ctg"; "sqrt"; "log2"; "^"; "+"; "-"; "|"|]

    member private this.Func2String (op: string) =
        match this.Operation with
        | "" -> string this.Value.Value
        | "x" -> "x"
        | "+" -> if this.IsComplexOperation op then 
                    $"""{this.Left.Value.Func2String "+"}+{this.Right.Value.Func2String "+"}""" 
                 else
                    $"""({this.Left.Value.Func2String "+"}+{this.Right.Value.Func2String "+"})"""
        | "-" -> if this.IsComplexOperation op then 
                    $"""{this.Left.Value.Func2String "-"}-{this.Right.Value.Func2String "-"}""" 
                 else
                    $"""({this.Left.Value.Func2String "-"}-{this.Right.Value.Func2String "-"})"""
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
        | "log2" -> $"""log2({this.Right.Value.Func2String "log2"})"""
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

// Get the bracket or module indexes.
let getBracketsIndexes (line: string) =
    // Get the index of the nearest bracket or module
    let getClosest leftBracket leftModule =
        if leftBracket <> -1 && (leftModule = -1 || 
           leftModule <> -1 && leftBracket < leftModule) then 
           (leftBracket, '(')
        elif leftModule <> -1 && (leftBracket = -1  || 
             leftBracket <> -1 && leftModule < leftBracket) then 
             (leftModule, '|')
        else (-1, '-')

    let rec findBracketsIndexes total current =
        if total = 0 then current - 1
        elif line[current] = '(' then findBracketsIndexes (total + 1) (current + 1)
        elif line[current] = ')' then findBracketsIndexes (total - 1) (current + 1)
        else findBracketsIndexes total (current + 1)

    let rec findModulesIndexes total current lastClose =
        if total = 0 then
            current - 1
        elif line[current] = '|' then
            if not lastClose && line[current - 1] <> ')' && Regex.IsMatch(string line[current - 1], @"\W") then
                findModulesIndexes (total + 1) (current + 1) false
            else
                findModulesIndexes (total - 1) (current + 1) true
        else 
            findModulesIndexes total (current + 1) false

    let (left, operation) = getClosest (line.IndexOf '(') (line.IndexOf '|')

    match operation with
    | '(' -> (left, findBracketsIndexes 1 (left + 1))
    | '|' -> (left, findModulesIndexes 1 (left + 1) false)
    | _ -> (-1, -1)

// Gets the character index, after passing various checks.
let getSymbolIndex (left: int) (right: int) (symbol: char) (line: string)  =
    // Looking for a symbol outside the brackets
    let rec lookOutside (current: int) : int =
        if (left, right) = (-1, -1) then 
            line.IndexOf(symbol)
        elif current = line.Length then 
            -1
        else
            if line[current] = symbol then
                current
            // Skip inner
            elif current = left then
                lookOutside (current + right - left + 1)
            else
                lookOutside (current + 1)
    
    // Check that the symbol is not in other brackets
    let rec checkRightSide (line: string) (rLast: int) (rCurrent: int) index =
        let subLine = line[rLast + 2..]
        let (left, right) = getBracketsIndexes subLine
        
        let offset = index - rCurrent - 2
        
        if left = -1 || right = -1 || left > offset then
            index
        elif offset < right then
            -1
        else
            checkRightSide subLine right (right + rCurrent + 2) index
    
    let index = lookOutside 0

    // If found symbol beyond expression
    if right <> -1 && index > right + 1 then
        checkRightSide line right right index
    // If a negative number at the beginning
    elif index = 0 && symbol = '-' then
        let result = checkRightSide line right right (line[1..line.Length - 1].IndexOf '-')
        if result = -1 then result
        else result + 1
    else
        index

 // Removes all unnecessary brackets
let rec removeExtraBrackets (line: string) =
    let args = getBracketsIndexes line

    if line[0] = '(' && args = (0, line.Length - 1) then
        removeExtraBrackets line[1..line.Length - 2]
    else
        (new string(line), args)

// Splitting a string by operations.
let rec breakLine (brackets: int * int) (symbol: char) (line: string) =
    let (lbracket, rbracket) = brackets

    let index = getSymbolIndex lbracket rbracket symbol line

    let comp = lazy(line[rbracket..rbracket + 1].IndexOf symbol)

    // If symbol not found
    if index = -1 then
        ("-", "-")
    // If multiply, divide and pow (right side only)
    elif lbracket <> -1 && lbracket < index && symbol <> '+' && symbol <> '-' && comp.Force() <> -1 then
        (line[0..rbracket], line[rbracket + 2..])
    // All other operations (left side only)
    else
        (line[..index - 1], line[index + 1..])

// Converts a function represented in a string into a recursive Node entry.
let convertToFunc (line: string) : Node =
    let symbols = [|'+';'-';'*';'/';'^'|]

    // Looking for symbols which can split the function
    let rec searchSymbol (operands: string * string) (line: string) (brackets: int * int) (item: int) =
        match operands with 
        | ("-", "-") -> searchSymbol (breakLine brackets (symbols[item + 1]) line) line brackets (item + 1)
        | (val1, val2) -> (val1, val2, item)

    let rec convert (line: string) =

        // Removing extra brackets
        let (removed, (l, r)) = removeExtraBrackets line

        // printfn "l: %d r: %d => %s" l r removed

        match removed with 
        | "x" ->  { Value = None;         Operation = "x"; Left = None; Right = None; }
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
                | op when op.StartsWith("log2")-> { Value = None; Operation = "log2"; Right = Some(convert(op[5..r - 1])); Left = None; }
                | _ -> let (lft, rght, i) = searchSymbol (breakLine (l, r) symbols[0] removed) removed (l, r) 0
                       { Value = None; Operation = string symbols[i]; Left = Some(convert(lft)); Right = Some(convert(rght)) }
        | _ -> let (lft, rght, i) = searchSymbol (breakLine (l, r) symbols[0] removed) removed (l, r) 0
               { Value = None; Operation = string symbols[i]; Left = Some(convert(lft)); Right = Some(convert(rght)) }

    convert (line.Replace(" ", ""))      

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
        | "log2" -> Math.Log2(calculateFunc node.Right.Value x)
        | "tg"   -> Math.Tan(calculateFunc node.Right.Value x)
        | "ctg"  -> 1. / Math.Tan(calculateFunc node.Right.Value x)
        | "|"    -> Math.Abs(calculateFunc node.Right.Value x)
        | "^"    -> Math.Pow(calculateFunc node.Left.Value x, calculateFunc node.Right.Value x)
        | "*"    -> calculateFunc node.Left.Value x * calculateFunc node.Right.Value x
        | "/"    -> calculateFunc node.Left.Value x / calculateFunc node.Right.Value x
        | "+"    -> calculateFunc node.Left.Value x + calculateFunc node.Right.Value x
        | "-"    -> calculateFunc node.Left.Value x - calculateFunc node.Right.Value x
        | _ -> failwith "Not available"

let simplifyFunc (node: Node) =
    let rec searchSimple (node: Node) (simple: bool) (value: float) (deep: int) (negative: bool) =
        if node.Value.IsSome then 
            if negative then
                (value - node.Value.Value, deep)
            else
                (value + node.Value.Value, deep)
        else
            match node.Operation with
            | "+" -> if node.Right.Value.Value.IsSome then
                         searchSimple (node.Left.Value) true (value + node.Right.Value.Value.Value) (deep) false 
                     elif node.Left.Value.Value.IsSome then
                         searchSimple (node.Right.Value) true (value + node.Left.Value.Value.Value) (deep) false 
                     else 
                         searchSimple (node.Left.Value) true (value) (deep) false
            | "-" -> if node.Left.Value.Value.IsSome then 
                         if negative then
                            searchSimple (node.Right.Value) true (value - node.Left.Value.Value.Value) (deep) true
                         else
                            searchSimple (node.Right.Value) true (value + node.Left.Value.Value.Value) (deep) true
                     elif node.Right.Value.Value.IsSome then 
                         if negative then
                            searchSimple (node.Left.Value) true (value - node.Right.Value.Value.Value) (deep) true
                         else
                            searchSimple (node.Left.Value) true (value + node.Right.Value.Value.Value) (deep) true
                     else 
                         searchSimple (node.Right.Value) true (value) (deep) true
            | _ ->   if simple then (value, deep)
                     else searchSimple (node.Right.Value) false (value) (deep + 1) false

    let mutable clear = true

    let rec simple (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome) with
        | (true, true) -> if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
                              clear <- false
                              { Value = Some(calculateFunc node 0); Operation = ""; Left = None; Right = None }
                          else
                              { Value = None; Operation = node.Operation; Left = Some(simple node.Left.Value); Right = Some(simple node.Right.Value) }
        | (false, true) -> { Value = None; Operation = node.Operation; Left = None; Right = Some(simple node.Right.Value) }
        | (true, false) -> { Value = None; Operation = node.Operation; Left = Some(simple node.Left.Value); Right = None }
        | _ -> node

    let rec clearFunc (result: Node) =
        if clear then
            result
        else
            clear <- true
            clearFunc (simple result)
        
    clearFunc (simple node)