module Function

open System
open System.Globalization
open System.Text.RegularExpressions

let funcs = [|"ln"; "lg"; "sin"; "cos"; "sqrt"; "log2"; "tg"; "ctg"; "exp"; "arcsin"; 
"arccos"; "arctg"; "arcctg"; "sh"; "ch"; "th"; "cth"; "sch"; "csch"|]

type Node = 
    {
    Value: Option<float>
    Operation: string
    Left: Option<Node>
    Right: Option<Node>
    }

    member this.IsFunction() =
        this.Value.IsNone && this.Operation <> "x" && this.Operation <> "e" && this.Operation <> "|" &&
        this.Operation <> "pi" && not (this.IsComplexFunction())

    member this.IsComplexFunction() =
        Array.contains this.Operation funcs

    member this.ToRecord() =
        let getData (value: Option<'a>) =
            if value.IsSome then "Some(" + value.Value.ToString() + ")"
            else "None"

        $"{{Value = {getData(this.Value)};Operation = \"{this.Operation}\";Left = {getData(this.Left)};Right = {getData(this.Right)};}}"

    override this.ToString() =
        match this.Operation with
        | "" -> if this.Value.Value < 0. then
                    $"({this.Value.Value})"
                else
                    string this.Value.Value
        | "x" -> "x"
        | "pi" -> "pi"
        | "e" -> "e"
        //(|1-x|)
        | "+" -> $"{this.Left.Value.ToString()}+{this.Right.Value.ToString()}"
        | "-" -> if this.Right.Value.IsFunction() then
                    $"{this.Left.Value.ToString()}-({this.Right.Value.ToString()})"
                 else
                    $"{this.Left.Value.ToString()}-{this.Right.Value.ToString()}"
        | "*" -> if this.Left.Value.IsFunction() && this.Right.Value.IsFunction() then
                    $"({this.Left.Value.ToString()})*({this.Right.Value.ToString()})"
                 elif this.Left.Value.IsFunction() then
                    $"({this.Left.Value.ToString()})*{this.Right.Value.ToString()}"
                 elif this.Right.Value.IsFunction() then
                    $"{this.Left.Value.ToString()}*({this.Right.Value.ToString()})"
                 else
                    $"{this.Left.Value.ToString()}*{this.Right.Value.ToString()}"
        | "/" -> if this.Left.Value.IsFunction() && this.Right.Value.IsFunction() then
                    $"({this.Left.Value.ToString()})/({this.Right.Value.ToString()})"
                 elif this.Left.Value.IsFunction() then
                    $"({this.Left.Value.ToString()})/{this.Right.Value.ToString()}"
                 elif this.Right.Value.IsFunction() then
                    $"{this.Left.Value.ToString()}/({this.Right.Value.ToString()})"
                 else
                    $"{this.Left.Value.ToString()}/{this.Right.Value.ToString()}"
        | "^" -> if this.Left.Value.IsFunction() && this.Right.Value.IsFunction() then
                     $"({this.Left.Value.ToString()})^({this.Right.Value.ToString()})"
                 elif this.Left.Value.IsFunction() then
                     $"({this.Left.Value.ToString()})^{this.Right.Value.ToString()}"
                 elif this.Right.Value.IsFunction() then
                     $"{this.Left.Value.ToString()}^({this.Right.Value.ToString()})"
                 else
                     $"{this.Left.Value.ToString()}^{this.Right.Value.ToString()}"
        | "|"    -> $"|{this.Right.Value.ToString()}|"
        | val1 when Array.exists (fun x -> val1 = x) funcs -> $"{Array.find (fun x -> val1 = x) funcs}({this.Right.Value.ToString()})"
        | _ -> failwith "Not working"

let isNumber (number) =
    let mutable temp = 0.0
    // Parse with a dot
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

// Get the index of the nearest bracket or module
let getClosest rightBracket leftModule =
    if rightBracket <> -1 && (leftModule = -1 || 
        leftModule <> -1 && rightBracket > leftModule) then 
        (rightBracket, ')')
    elif leftModule <> -1 && (rightBracket = -1  || 
            rightBracket <> -1 && leftModule > rightBracket) then 
            (leftModule, '|')
    else (-1, '-')

let rec findBracketsIndexes (line: string) total current inc =
    if total = 0 then current - inc
    elif line[current] = '(' then findBracketsIndexes line (total - 1) (current + inc) inc
    elif line[current] = ')' then findBracketsIndexes line (total + 1) (current + inc) inc
    else findBracketsIndexes line total (current + inc) inc

let rec findModulesIndexes (line: string) total current lastClose inc =
    if total = 0 then
        current - inc
    elif current = line.Length - 1 || current = 0 then
        current
    elif line[current] = '|' then
        if not lastClose && line[current - 1] <> ')' &&  Regex.IsMatch(string line[current - 1], @"\W") then
            findModulesIndexes line (total + 1) (current + inc) false inc
        else
            findModulesIndexes line (total - 1) (current + inc) true inc
    else 
        findModulesIndexes line total (current + inc) false inc

// Get the bracket or module indexes.
let getBracketsIndexesRight2Left (line: string) =
    let (left, operation) = getClosest (line.IndexOf '(') (line.IndexOf '|')

    match operation with
    | ')' -> (left, findBracketsIndexes line -1 (left + 1) 1)
    | '|' -> (left, findModulesIndexes line 1 (left + 1) false 1)
    | _ -> (-1, -1)

let getBracketsIndexesLeft2Right (line: string) =
    let (right, operation) = getClosest (line.LastIndexOf ')') (line.LastIndexOf '|')

    match operation with
    | ')' -> (findBracketsIndexes line 1 (right - 1) -1, right)
    | '|' -> (findModulesIndexes line -1 (right - 1) true -1, right)
    | _ -> (-1, -1)

// Gets the character index, after passing various checks.
let getSymbolIndexRight2Left (left: int) (right: int) (symbol: char) (line: string)  =
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
        let (left, right) = getBracketsIndexesRight2Left subLine
        
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
    else
        index

let getSymbolIndexLeft2Right (left: int) (right: int) (symbol: char) (line: string)  =
    // Looking for a symbol outside the brackets
    let rec lookOutside (current: int) : int =
        if (left, right) = (-1, -1) then 
            line.LastIndexOf(symbol)
        elif current < 0 then 
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
        let (left, right) = getBracketsIndexesLeft2Right subLine
        
        if left = -1 || right = -1 || index > right then
            index
        elif left < index then
            -1
        else
            checkLeftSide subLine left index
    
    let index = lookOutside (line.Length - 1)

    // If found symbol beyond expression
    if left <> -1 && index < left && index <> 0 then
        checkLeftSide line left index
    else
        index

 // Removes all unnecessary brackets
let rec removeExtraBrackets (line: string) =
    let args = getBracketsIndexesLeft2Right line

    if line[0] = '(' && args = (0, line.Length - 1) then
        removeExtraBrackets line[1..line.Length - 2]
    else
        (new string(line), args)

// Splitting a string by operations.
let rec breakLine (brackets: int * int) (symbol: char) (line: string) =
    let (lbracket, rbracket) = brackets

    let index = lazy (
        if symbol = '^' then
            getSymbolIndexRight2Left lbracket rbracket symbol line
        else
            getSymbolIndexLeft2Right lbracket rbracket symbol line
    )

    let comp = lazy(line[rbracket..rbracket + 1].LastIndexOf symbol)

    // Not sure
    // If complex function (cos, sin, log, ...) then -sin => -1 * sin
    if line[0] = '-' && symbol = '*' && Regex.IsMatch(string line[1], @"[a-z]") then
        ("-1", line[1..])
    // If symbol not found
    elif index.Force() = -1 then
        ("-", "-")
    // If -number
    elif symbol = '-' && index.Force() = 0 then
        ("0", line[1..])
    // If multiply, divide and pow (right side only) ???
    elif lbracket <> -1 && lbracket < index.Force() && symbol <> '+' && symbol <> '-' && comp.Force() <> -1 then
        (line[0..rbracket], line[rbracket + 2..])
    // All other operations
    else
        (line[..index.Force() - 1], line[index.Force() + 1..])

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
        | "x" ->  { Value = None; Operation = "x"; Left = None; Right = None; }
        | "pi" -> { Value = None; Operation = "pi"; Left = None; Right = None; }
        | "e" ->  { Value = None; Operation = "e"; Left = None; Right = None; }
        | val1 when isNumber(val1) -> { Value = Some(float val1); Operation = ""; Right = None; Left = None }
        | val1 when r = val1.Length - 1 && val1[r] = '|' && l = 0 && val1[0] = '|' -> 
            { Value = None; Operation = "|"; Right = Some(convert(val1[1..val1.Length - 2])); Left = None; }
        | val1 when r = val1.Length - 1 && l > 0 -> 
                let result = Array.tryFind (fun (x: string) -> val1.StartsWith(x)) funcs
                
                match result with
                | Some x when x.Length = l -> { Value = None; Operation = x; Left = None; Right = Some(convert(val1[l + 1..r - 1])); }
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
        | "x"  -> x
        | "pi" -> Math.PI
        | "e"  -> Math.E
        | "+"    -> calculateFunc node.Left.Value x + calculateFunc node.Right.Value x
        | "-"    -> calculateFunc node.Left.Value x - calculateFunc node.Right.Value x
        | "*"    -> calculateFunc node.Left.Value x * calculateFunc node.Right.Value x
        | "/"    -> calculateFunc node.Left.Value x / calculateFunc node.Right.Value x
        | "^"    -> Math.Pow(calculateFunc node.Left.Value x, calculateFunc node.Right.Value x)
        | "|"    -> Math.Abs(calculateFunc node.Right.Value x)
        | "ln"   -> Math.Log(calculateFunc node.Right.Value x)
        | "lg"   -> Math.Log10(calculateFunc node.Right.Value x)
        | "sin"  -> Math.Sin(calculateFunc node.Right.Value x)
        | "cos"  -> Math.Cos(calculateFunc node.Right.Value x)
        | "sqrt" -> Math.Sqrt(calculateFunc node.Right.Value x)
        | "log2" -> Math.Log2(calculateFunc node.Right.Value x)
        | "tg"   -> Math.Tan(calculateFunc node.Right.Value x)
        | "ctg"  -> 1. / Math.Tan(calculateFunc node.Right.Value x)
        | "exp"  -> Math.Exp (calculateFunc node.Right.Value x)
        | "arcsin"  -> Math.Asin (calculateFunc node.Right.Value x)
        | "arccos"  -> Math.Acos (calculateFunc node.Right.Value x)
        | "arctg"  ->  Math.Atan (calculateFunc node.Right.Value x)
        | "arcctg"  -> Math.PI / 2. - Math.Atan (calculateFunc node.Right.Value x)
        | "sh"  -> Math.Sinh (calculateFunc node.Right.Value x)
        | "ch"  -> Math.Cosh (calculateFunc node.Right.Value x)
        | "th"  -> Math.Tanh (calculateFunc node.Right.Value x)
        | "cth"  -> 1. / Math.Tanh (calculateFunc node.Right.Value x)
        | "sch"  -> 1. / Math.Cosh (calculateFunc node.Right.Value x)
        | "csch"  -> 1. / Math.Sinh (calculateFunc node.Right.Value x)
        | _ -> failwith "Not available"



// In developing...

let rec derivativeFunc (node: Node) : Node =
    if node.Value.IsSome then 
        { Value = Some(0.); Operation = ""; Right = None; Left = None }
    else
        match node.Operation with
        | "x" -> { Value = Some(1.); Operation = ""; Right = None; Left = None }
        | "^"    -> if node.Right.Value.Value.IsSome then
                        convertToFunc $"(({derivativeFunc node.Left.Value})*({node.Right.Value})*({node.Left.Value})^({node.Right.Value.Value.Value}-1))"
                    elif node.Left.Value.Value.IsSome then
                        convertToFunc $"(({derivativeFunc node.Right.Value})*({node.Left.Value})^({node.Right.Value})*ln({node.Left.Value}))"
                    else
                        let hard = convertToFunc $"ln({node.Left.Value})"
                        let symbol = "*"
                        convertToFunc $"e^(({node.Right.Value})*ln({node.Left.Value}))*({derivativeFunc { Value = None; Operation = symbol; Left = Some(node.Left.Value); Right = Some(hard)}})"
        | "*"    -> convertToFunc $"(({derivativeFunc node.Left.Value})*({node.Right.Value}))+(({node.Left.Value})*({derivativeFunc node.Right.Value}))"
        | "/"    -> convertToFunc $"(({derivativeFunc node.Left.Value})*({node.Right.Value})-({node.Left.Value})*({derivativeFunc node.Right.Value}))/({node.Right.Value})^2"
        | "+"    -> convertToFunc $"{derivativeFunc node.Left.Value}+{derivativeFunc node.Right.Value}"
        | "-"    -> convertToFunc $"{derivativeFunc node.Left.Value}-({derivativeFunc node.Right.Value})"
        | "ln"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/({node.Right.Value})"
        | "sin"    -> convertToFunc $"(({derivativeFunc node.Right.Value})*cos({node.Right.Value}))"
        | "cos"    -> convertToFunc $"(({derivativeFunc node.Right.Value})*(-sin({node.Right.Value})))"
        | "tg"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/(cos({node.Right.Value})^2)"
        | "ctg"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/(-sin({node.Right.Value})^2)"
        | "sqrt"    -> convertToFunc $"({derivativeFunc node.Right.Value})*1/(2*sqrt({node.Right.Value}))"
        | _ -> failwith "Not available"

let simplifyFunc (node: Node) =
    let mutable clear = true
    let mutable nt = 0.

    let change (op: string) (number: float) =
        if op = "-" then
            -number
        else
            number

    let rec middle (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" -> 
            if node.Left.Value.Value.IsSome then
                nt <- nt + node.Left.Value.Value.Value
                node.Right.Value
            elif node.Right.Value.Value.IsSome then
                nt <- nt + change node.Operation node.Right.Value.Value.Value
                node.Left.Value
            else
                { Value = None; Operation = node.Operation; Left = Some(middle node.Left.Value); Right = Some(middle node.Right.Value) }
        | (false, true, _) -> { Value = None; Operation = node.Operation; Left = None; Right = Some(middle node.Right.Value) }
        | _ -> node

    let rec simple (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome) with
        | (true, true) -> if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
                              clear <- false
                              { Value = Some(calculateFunc node 0); Operation = ""; Left = None; Right = None }
                          elif node.Operation = "*" && ((node.Left.Value.Value.IsSome && node.Left.Value.Value.Value = 0.) || 
                              (node.Right.Value.Value.IsSome && node.Right.Value.Value.Value = 0.)) then
                              clear <- false
                              { Value = Some(0.); Operation = ""; Left = None; Right = None }
                              // ^^^^^^^^^^
                          elif (node.Operation = "*" && node.Left.Value.Value.IsSome && node.Left.Value.Value.Value = 1.) then
                              clear <- false
                              node.Right.Value 
                          elif (node.Operation = "*" && node.Right.Value.Value.IsSome && node.Right.Value.Value.Value = 1.) then
                              clear <- false
                              node.Left.Value
                          else
                              { Value = None; Operation = node.Operation; Left = Some(simple node.Left.Value); Right = Some(simple node.Right.Value) }
        | (false, true) -> { Value = None; Operation = node.Operation; Left = None; Right = Some(simple node.Right.Value) }
        | _ -> node

    let rec clearFunc (result: Node) =
        if clear then
            result
        else
            clear <- true
            clearFunc (simple result)
        
    clearFunc (simple node)