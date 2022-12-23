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
        Array.contains op [|"ln"; "lg"; "sin"; "cos"; "tg"; "ctg"; "sqrt"; "log2"; "^"; "+"; "|"|]

    member private this.Func2String (op: string) =
        match this.Operation with
        | "" -> if this.Value.Value < 0. then
                    $"({this.Value.Value})"
                else
                    string this.Value.Value
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
    let getClosest rightBracket leftModule =
        if rightBracket <> -1 && (leftModule = -1 || 
           leftModule <> -1 && rightBracket > leftModule) then 
           (rightBracket, ')')
        elif leftModule <> -1 && (rightBracket = -1  || 
             rightBracket <> -1 && leftModule > rightBracket) then 
             (leftModule, '|')
        else (-1, '-')

    let rec findBracketsIndexes total current =
        if total = 0 then current + 1
        elif line[current] = '(' then findBracketsIndexes (total - 1) (current - 1)
        elif line[current] = ')' then findBracketsIndexes (total + 1) (current - 1)
        else findBracketsIndexes total (current - 1)

    let rec findModulesIndexes total current lastClose =
        if total = 0 then
            current + 1
        elif current = 1 then
            current - 1
        elif line[current] = '|' then
            if not lastClose && line[current - 1] <> ')' && Regex.IsMatch(string line[current - 1], @"\W") then
                findModulesIndexes (total + 1) (current - 1) false
            else
                findModulesIndexes (total - 1) (current - 1) true
        else 
            findModulesIndexes total (current - 1) false

    let (right, operation) = getClosest (line.LastIndexOf ')') (line.LastIndexOf '|')

    match operation with
    | ')' -> (findBracketsIndexes 1 (right - 1), right)
    | '|' -> (findModulesIndexes -1 (right - 1) false, right)
    | _ -> (-1, -1)

// Gets the character index, after passing various checks.
let getSymbolIndex (left: int) (right: int) (symbol: char) (line: string)  =
    // Looking for a symbol outside the brackets
    let rec lookOutside (current: int) : int =
        if (left, right) = (-1, -1) then 
            line.LastIndexOf(symbol)
        elif current <= 0 then 
            -1
        else
            if line[current] = symbol then
                current
            // Skip inner
            elif current = right then
                lookOutside (current - (right - left) - 1)
            else
                lookOutside (current - 1)
    
    // Check that the symbol is not in other brackets
    let rec checkLeftSide (line: string) (lLast: int) index =
        let subLine = line[..lLast - 2]
        let (left, right) = getBracketsIndexes subLine
        
        if left = -1 || right = -1 || index > right then
            index
        elif left < index then
            -1
        else
            checkLeftSide subLine left index
    
    let index = lookOutside (line.Length - 1)

    // If found symbol beyond expression
    if left <> -1 && index < left then
        checkLeftSide line left index
    // If a negative number at the beginning
    //elif index = 0 && symbol = '-' then
    //    let result = checkLeftSide line right (line[1..line.Length - 1].LastIndexOf '-')
    //    if result = -1 then result
    //    else result + 1
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

    let comp = lazy(line[rbracket..rbracket + 1].LastIndexOf symbol)


    if line[0] = '-' && symbol = '*' && Regex.IsMatch(string line[1], @"[a-z]") then
        ("-1", line[1..])
    // If symbol not found
    elif index = -1 then
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

        //printfn "l: %d r: %d => %s" l r removed

        match removed with 
        | "x" ->  { Value = None;         Operation = "x"; Left = None; Right = None; }
        | "pi" -> { Value = Some(Math.PI); Operation = ""; Left = None; Right = None; }
        | "e" ->  { Value = Some(Math.E);  Operation = ""; Left = None; Right = None; }
        | val1 when isNumber(val1) -> { Value = Some(float val1); Operation = ""; Right = None; Left = None }
        | val1 when r = val1.Length - 1 && val1[r] = '|' && val1[0] = '|' -> 
            { Value = None; Operation = "|"; Right = Some(convert(val1[1..val1.Length - 2])); Left = None; }
        | val1 when r = val1.Length - 1 && l > 0 -> 
                match val1 with
                | op when op.StartsWith("ln")   && l = 2 -> { Value = None; Operation = "ln";   Left = None; Right = Some(convert(op[3..r - 1])); }
                | op when op.StartsWith("lg")   && l = 2 -> { Value = None; Operation = "lg";   Left = None; Right = Some(convert(op[3..r - 1])); }        
                | op when op.StartsWith("tg")   && l = 2 -> { Value = None; Operation = "tg";   Left = None; Right = Some(convert(op[3..r - 1])); }
                | op when op.StartsWith("sin")  && l = 3 -> { Value = None; Operation = "sin";  Left = None; Right = Some(convert(op[4..r - 1])); }
                | op when op.StartsWith("cos")  && l = 3 -> { Value = None; Operation = "cos";  Left = None; Right = Some(convert(op[4..r - 1])); }
                | op when op.StartsWith("ctg")  && l = 3 -> { Value = None; Operation = "ctg";  Left = None; Right = Some(convert(op[4..r - 1])); }
                | op when op.StartsWith("sqrt") && l = 4 -> { Value = None; Operation = "sqrt"; Left = None; Right = Some(convert(op[5..r - 1])); }
                | op when op.StartsWith("log2") && l = 4 -> { Value = None; Operation = "log2"; Left = None; Right = Some(convert(op[5..r - 1])); }
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

let rec derivativeFunc (node: Node) : Node =
    if node.Value.IsSome then 
        { Value = Some(0.); Operation = ""; Right = None; Left = None }
    else
        match node.Operation with
        | "x" -> { Value = Some(1.); Operation = ""; Right = None; Left = None }
        | "^"    -> if node.Right.Value.Value.IsSome then
                        convertToFunc $"({node.Right.Value})*({node.Left.Value})^({node.Right.Value.Value.Value}-1)*({derivativeFunc node.Left.Value})"
                    else
                        let hard = convertToFunc $"ln({node.Left.Value})"
                        let symbol = "*"
                        convertToFunc $"e^({node.Right.Value}*ln({node.Left.Value})*({derivativeFunc { Value = None; Operation = symbol; Left = Some(node.Left.Value); Right = Some(hard)}}))"
        | "*"    -> convertToFunc $"(({derivativeFunc node.Left.Value})*({node.Right.Value}))+(({node.Left.Value})*({derivativeFunc node.Right.Value}))"
        | "/"    -> convertToFunc $"(({derivativeFunc node.Left.Value})*({node.Right.Value})-({node.Left.Value})*({derivativeFunc node.Right.Value}))/({node.Right.Value})^2"
        | "+"    -> convertToFunc $"{derivativeFunc node.Left.Value}+{derivativeFunc node.Right.Value}"
        | "-"    -> convertToFunc $"{derivativeFunc node.Left.Value}-{derivativeFunc node.Right.Value}"
        | "ln"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/({node.Right.Value})"
        | "sin"    -> convertToFunc $"({derivativeFunc node.Right.Value})*cos({node.Right.Value})"
        | "cos"    -> convertToFunc $"({derivativeFunc node.Right.Value})*(-sin({node.Right.Value}))"
        | "tg"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/(cos({node.Right.Value})^2)"
        | "ctg"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/(-sin({node.Right.Value})^2)"
        | "sqrt"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/(2*sqrt({node.Right.Value}))"
        | _ -> failwith "Not available"


// wait
let rec simpleStart (node: Node) : Node =
        if node.Value.IsSome || node.Operation = "x" then
            node
        else
            // NOT WORKING WITH COMPLEX FUNCTIONS
            match (node.Left.Value.Value.IsSome, node.Right.Value.Value.IsSome, node.Operation) with
            | (true, true, "*") -> match (node.Left.Value.Value.Value, node.Right.Value.Value.Value) with
                              | (1., _) -> { Value = node.Right.Value.Value; Operation = ""; Left = None; Right = None }
                              | (_, 1.) -> { Value = node.Left.Value.Value; Operation = ""; Left = None; Right = None }
                              | (0., _) | (_, 0.) -> { Value = Some(0.); Operation = ""; Left = None; Right = None }
                              | _ -> node
            | (false, true, "*") -> if node.Right.Value.Value.Value = 0. then
                                        { Value = Some(0.); Operation = ""; Left = None; Right = None }
                                    elif node.Right.Value.Value.Value = 1. then
                                        node.Left.Value
                                    else
                                        { Value = None; Operation = node.Operation; Left = Some(simpleStart node.Left.Value); Right = node.Right }
            | (true, false, "*") -> if node.Left.Value.Value.Value = 0. then
                                        { Value = Some(0.); Operation = ""; Left = None; Right = None }
                                    elif node.Left.Value.Value.Value = 1. then
                                        node.Right.Value
                                    else
                                        { Value = None; Operation = node.Operation; Left = node.Left; Right = Some(simpleStart node.Right.Value) }
            | _ -> { Value = None; Operation = node.Operation; Left = Some(simpleStart node.Left.Value); Right = Some(simpleStart node.Right.Value) }

// wait
let simplifyFunc (node: Node) =
    let mutable clear = false

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
        
    clearFunc (simpleStart (simple node))