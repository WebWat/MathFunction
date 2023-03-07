module Node

open System
open System.Globalization

exception UnknownOperation of string
exception ArgumentNotExist of string
exception InvalidBracket of string
exception InvalidModule of string

// 0000 - 009F
let funcsMap =
    Map [ ("ln"    , '\u0010')
          ("lg"    , '\u0011')
          ("sin"   , '\u0012')
          ("cos"   , '\u0013')
          ("sqrt"  , '\u0014')
          ("log2"  , '\u0015')
          ("tg"    , '\u0016')
          ("ctg"   , '\u0017')
          ("exp"   , '\u0018')
          ("arcsin", '\u0019')
          ("arccos", '\u001A')
          ("arctg" , '\u001B')
          ("arcctg", '\u001C')
          ("sgn"   , '\u001D')]
let funcs = Map.toArray funcsMap |> Array.map snd

let allowedArgs = [| 'a'; 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'i'; 'g'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z' |]

let constsMap = 
    Map [ (""  , '\u0000')
          ("pi", '\u0001')
          ("e" , '\u0002')]
let consts =  Map.toArray constsMap |> Array.map snd

let operations = [| '+'; '-'; '*'; '/'; '^' |]

let symbols = Array.append operations [|'('; '|'|]

type Node =
    { Value: float
      Operation: char
      Left: Option<Node>
      Right: Option<Node> }
    
    member this.ToMathJax() =
        let summary = 
            [||] 
            |> Array.append consts
            |> Array.append allowedArgs
            |> Array.append funcs
        
        let additional = [|'*'; '/'|] 

        let summaryComp = 
            additional
            |> Array.append summary

        let needBracketsSum (operation: char) =
            operation <> '|'
            && operation <> '^'
            && not (Array.contains operation summary)

        let needBracketsComp (operation: char) =
            operation <> '|'
            && operation<> '^'
            && not (Array.contains operation summaryComp)
        let rec convert2math node =
            match node.Operation with
            | '\u0000' -> string node.Value
            | '\u0001' -> "\\pi "
            | '\u0002' -> "e"
            | '+' -> convert2math node.Left.Value + "+" + convert2math node.Right.Value
            | '-' ->
                if node.Left.Value.Operation = '\u0000' && node.Left.Value.Value = 0 then
                    if
                        needBracketsComp node.Right.Value.Operation
                    then
                        "-\\left(" + convert2math node.Right.Value + "\\right)"
                    else
                        "-" + convert2math node.Right.Value
                else if
                    needBracketsComp node.Right.Value.Operation
                then
                    convert2math node.Left.Value + "-\\left(" + convert2math node.Right.Value + "\\right)"
                else
                    convert2math node.Left.Value + "-" + convert2math node.Right.Value
            | '|' -> "\\left|" + convert2math node.Right.Value + "\\right|"
            | '*' ->
                let left = needBracketsComp node.Left.Value.Operation
                let right = needBracketsComp node.Right.Value.Operation

                if
                    left && right
                then
                    "\\left(" + convert2math node.Left.Value + "\\right)\\cdot\\left(" + convert2math node.Right.Value + "\\right)"
                elif
                    left
                then
                    "\\left(" + convert2math node.Left.Value + "\\right)\\cdot" + convert2math node.Right.Value
                elif
                    right
                then
                    convert2math node.Left.Value + "\\cdot\\left(" + convert2math node.Right.Value+ "\\right)"
                else
                    convert2math node.Left.Value + convert2math node.Right.Value
            | '/' ->
                let left = needBracketsComp node.Left.Value.Operation
                let right = needBracketsComp node.Right.Value.Operation

                if
                    left && right
                then
                    "\\frac{\\left(" + convert2math node.Left.Value + "\\right)}{\\left(" + convert2math node.Right.Value + "\\right)}"
                elif
                    left
                then
                    "\\frac{\\left(" + convert2math node.Left.Value + "\\right)}{" + convert2math node.Right.Value + "}"
                elif 
                    right || Array.contains node.Right.Value.Operation additional
                then
                    "\\frac{" + convert2math node.Left.Value + "}{\\left(" + convert2math node.Right.Value + "\\right)}"
                else
                    "\\frac{" + convert2math node.Left.Value + "}{" + convert2math node.Right.Value + "}"
            | '^' ->
                let left = needBracketsSum node.Left.Value.Operation
                let right = needBracketsSum node.Right.Value.Operation

                if left && right then
                    "\\left(" + convert2math node.Left.Value + ")^{\\left(" + convert2math node.Right.Value + "\\right)}"
                elif left then
                    "\\left(" + convert2math node.Left.Value + "\\right)^{" + convert2math node.Right.Value + "}"
                elif right then
                     convert2math node.Left.Value + "^{\\left(" + convert2math node.Right.Value + "\\right)}"
                else
                     convert2math node.Left.Value + "^{" + convert2math node.Right.Value + "}"
            | val1 when 
                Array.contains val1 allowedArgs ->  
                    string val1
            | '\u0010' -> "\\ln\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0011' -> "\\log_{10}\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0012' -> "\\sin\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0013' -> "\\cos\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0014' -> "\\sqrt\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0015' -> "\\log_{2}\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0016' -> "\\tan\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0017' -> "\\cot\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0018' -> "\\exp\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u0019' -> "\\arcsin\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u001A' -> "\\arccos\\left(" + convert2math node.Right.Value + "\\right)"
            | '\u001B' -> "\\arctan\\left(" + convert2math node.Right.Value + "\\right)"
            | _ -> raise (UnknownOperation (string node.Operation))

        convert2math this

    override this.ToString() =
        let summary = 
            [||] 
            |> Array.append consts
            |> Array.append allowedArgs
            |> Array.append funcs
        
        let additional = [|'*'; '/'|] 

        let summaryComp = 
            additional
            |> Array.append summary

        let needBracketsSum (operation: char) =
            operation <> '|'
            && operation <> '^'
            && not (Array.contains operation summary)

        let needBracketsComp (operation: char) =
            operation <> '|'
            && operation<> '^'
            && not (Array.contains operation summaryComp)

        let rec convert2str node =
            match node.Operation with
            | '\u0000'  ->
                if node.Value < 0. then
                    "(" + (string node.Value) + ")"
                else
                    string node.Value
            | '\u0001' -> "pi"
            | '\u0002' -> "e"
            | '+' -> convert2str node.Left.Value + "+" + convert2str node.Right.Value
            | '|' -> "|" + convert2str node.Right.Value + "|"
            | '-' ->
                if node.Left.Value.Operation = '\u0000' && node.Left.Value.Value = 0 then
                    if
                        needBracketsComp node.Right.Value.Operation
                    then
                        "-(" + convert2str node.Right.Value + ")"
                    else
                        "-" + convert2str node.Right.Value
                else if
                    needBracketsComp node.Right.Value.Operation
                then
                    convert2str node.Left.Value + "-(" + convert2str node.Right.Value + ")"
                else
                    convert2str node.Left.Value + "-" + convert2str node.Right.Value
            | '*' ->
                let left = needBracketsComp node.Left.Value.Operation
                let right = needBracketsComp node.Right.Value.Operation

                if
                    left && right
                then
                    "(" + convert2str node.Left.Value + ")*(" + convert2str node.Right.Value + ")"
                elif
                    left
                then
                    "(" + convert2str node.Left.Value + ")*" + convert2str node.Right.Value
                elif
                    right
                then
                    convert2str node.Left.Value + "*(" + convert2str node.Right.Value+ ")"
                else
                    convert2str node.Left.Value + "*" + convert2str node.Right.Value
            | '/' ->
                let left = needBracketsComp node.Left.Value.Operation
                let right = needBracketsComp node.Right.Value.Operation

                if
                    left && right
                then
                    "(" + convert2str node.Left.Value + ")/(" + convert2str node.Right.Value + ")"
                elif
                    left
                then
                    "(" + convert2str node.Left.Value + ")/" + convert2str node.Right.Value
                elif 
                    right || Array.contains node.Right.Value.Operation additional
                then
                     convert2str node.Left.Value + "/(" + convert2str node.Right.Value + ")"
                else
                     convert2str node.Left.Value + "/" + convert2str node.Right.Value
            | '^' ->
                let left = needBracketsSum node.Left.Value.Operation
                let right = needBracketsSum node.Right.Value.Operation

                if left && right then
                    "(" + convert2str node.Left.Value + ")^(" + convert2str node.Right.Value + ")"
                elif left then
                    "(" + convert2str node.Left.Value + ")^" + convert2str node.Right.Value
                elif right then
                     convert2str node.Left.Value + "^(" + convert2str node.Right.Value + ")"
                else
                     convert2str node.Left.Value + "^" + convert2str node.Right.Value
            | val1 when 
                Array.contains val1 allowedArgs ->  
                    string val1
            | val1 when Array.contains val1 funcs ->
                Map.findKey (fun _ v -> v = val1) funcsMap + "(" + convert2str node.Right.Value + ")"
            | _ -> raise (UnknownOperation (string node.Operation))

        convert2str this

type Operation =
    | Dec = -1
    | Inc = 1

let isComplexFunction (node: Node) = Array.contains node.Operation funcs

let isConst (node: Node) = Array.contains node.Operation consts

let isArg (node: Node) = Array.contains node.Operation allowedArgs

let getNumber (number: float) : Node = 
    { Value = number
      Operation = '\u0000'
      Left = None
      Right = None }

let getParameter (arg: char) : Node = 
    { Value = 0
      Operation = arg
      Left = None
      Right = None }

let getFunction (func: string) (node: Node) =
    { Value = 0
      Operation = (Map.find func funcsMap)
      Left = None
      Right = Some node }

let combine (op: char) (left: Node) (right: Node) =
    { Value = 0
      Operation = op
      Left = Some left
      Right = Some right }

let isNumber (number: string) (temp: outref<float>) =

    // Parse with a dot
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

// Get the index of the rightmost bracket or module.
let private getClosest rightBracket leftModule =
    if
        rightBracket <> -1
        && (leftModule = -1 || rightBracket > leftModule)
    then
        (rightBracket, ')')
    elif
        leftModule <> -1
        && (rightBracket = -1 || leftModule > rightBracket)
    then
        (leftModule, '|')
    else
        (-1, '-')

// Returns the first outer brackets indexes.
let rec private findBracketsIndexes (line: string) (total: int) (current: int) (inc: Operation) =
    if total = 0 then
        current - int inc
    elif current = -1 then
        raise (InvalidBracket line)
    elif line[current] = '(' then
        findBracketsIndexes line (total - 1) (current + int inc) inc
    elif line[current] = ')' then
        findBracketsIndexes line (total + 1) (current + int inc) inc
    else
        findBracketsIndexes line total (current + int inc) inc

// Returns the first outer modules indexes.
let rec private findModulesIndexes (line: string) (total: int) (current: int) (lastClose: bool) (inc: Operation) =
    if total = 0 then
        current - int inc
    elif current = line.Length - 1 || current = 0 then
        current
    elif current >= line.Length then
        raise (InvalidModule line)
    elif line[current] = '|' then
        if
            not lastClose
            && line[current - 1] <> ')'
            && Array.contains line[current - 1] symbols
        then
            findModulesIndexes line (total + 1) (current + int inc) false inc
        else
            findModulesIndexes line (total - 1) (current + int inc) true inc
    else
        findModulesIndexes line total (current + int inc) false inc

// Looking for a symbol outside the brackets.
let rec private lookOutside
    (line: string)
    (symbol: char)
    (leftBracket: int)
    (rightBracket: int)
    (current: int)
    (inc: Operation)
    : int =

    if leftBracket = -1 then
        if inc = Operation.Dec then
            line.LastIndexOf(symbol)
        else
            line.IndexOf(symbol)
    elif current = -1 || current = line.Length then
        -1
    else if line[current] = symbol then
        current
    // Skip inner
    elif inc = Operation.Dec && current = rightBracket then
        lookOutside line symbol leftBracket rightBracket (current - (rightBracket - leftBracket) + int inc) inc
    // Skip inner
    elif inc = Operation.Inc && current = leftBracket then
        lookOutside line symbol leftBracket rightBracket (current + (rightBracket - leftBracket) + int inc) inc
    else
        lookOutside line symbol leftBracket rightBracket (current + int inc) inc

// Get the bracket indexes.
let getBracketsIndexesRight2Left (line: string) =
    let (left, operation) = getClosest (line.IndexOf '(') (line.IndexOf '|')

    match operation with
    | ')' -> (left, findBracketsIndexes line -1 (left + 1) Operation.Inc)
    | '|' -> (left, findModulesIndexes line 1 (left + 1) false Operation.Inc)
    | _ -> (-1, -1)

let getBracketsIndexesLeft2Right (line: string) =
    let (right, operation) = getClosest (line.LastIndexOf ')') (line.LastIndexOf '|')

    match operation with
    | ')' -> (findBracketsIndexes line 1 (right - 1) Operation.Dec, right)
    | '|' -> (findModulesIndexes line -1 (right - 1) true Operation.Dec, right)
    | _ -> (-1, -1)

// Gets the character index, after passing various checks.

// Only for '^' operation.
let getSymbolIndexRight2Left (leftBracket: int) (rightBracket: int) (symbol: char) (line: string) =
    let index = lookOutside line symbol leftBracket rightBracket 0 Operation.Inc

    index

let getSymbolIndexLeft2Right (leftBracket: int) (rightBracket: int) (symbol: char) (line: string) =
    // Check that the symbol is not in other brackets
    let rec checkLeftSide (line: string) (lLast: int) index =
        let subLine = line[.. lLast - 2]
        let (left, right) = getBracketsIndexesLeft2Right subLine

        // If there are no brackets or the symbol between the brackets
        if left = -1 || right = -1 || (right < index && index < lLast) then
            index
        else
            checkLeftSide subLine left 
                <| lookOutside subLine symbol left right (subLine.Length - 1) Operation.Dec

    let index =
        lookOutside line symbol leftBracket rightBracket (line.Length - 1) Operation.Dec

    // If found symbol beyond expression
    if index <> -1 && leftBracket <> -1 && index < leftBracket + 1 then
        checkLeftSide line leftBracket index
    else
        index

// Removes all unnecessary brackets.
let rec removeExtraBrackets (line: string) =
    let args = getBracketsIndexesLeft2Right line

    if line[0] = '(' && args = (0, line.Length - 1) then
        removeExtraBrackets line[1 .. line.Length - 2]
    else
        (new string (line), args)

// Splitting a string by operations.
let rec breakLine (lbracket: int) (rbracket: int) (symbol: char) (line: string) =
    let index =
        if symbol = '^' then
            let (l, r) = getBracketsIndexesRight2Left line
            getSymbolIndexRight2Left l r symbol line
        else
            getSymbolIndexLeft2Right lbracket rbracket symbol line

    // If symbol not found
    if index = -1 then
        ("-", "-")
    // If -number
    elif symbol = '-' && index = 0 then
        ("0", line[1..])
    // If multiply, divide and pow (right side only) Ex: (-(2+2)*2)^2
    elif
        rbracket <> -1
        && rbracket < line.Length - 1
        && symbol <> '+' 
        && symbol <> '-'
        && line[rbracket + 1] = symbol
    then
        (line[..rbracket], line[rbracket + 2 ..])
    // All other operations
    else
        (line[.. index - 1], line[index + 1 ..])

// Looking for symbols which can split the function.
let rec searchSymbol (operands: string * string) (line: string) (leftBracket: int) (rightBracket: int) (item: int) =
    match operands with
    | ("-", "-") ->
        if item + 1 = operations.Length then
            raise (UnknownOperation line)
        else
            searchSymbol 
                (breakLine leftBracket rightBracket (operations[item + 1]) line) 
                line 
                leftBracket 
                rightBracket 
                (item + 1)
    | (val1, val2) -> (val1, val2, item)


// Converts a function represented in a string into a recursive Node entry.
let convertToFunc (line: string) : Node =
    let mutable temp = 0.

    let rec convert (line: string) =
        // Removing extra brackets
        let (removed, (l, r)) = removeExtraBrackets line

        //printfn "l: %d r: %d => %s" l r removed

        match removed with
        | val1 when 
            val1.Length = 1 && Array.contains (char val1) allowedArgs
            ->
            { Value = 0
              Operation = (char val1)
              Left = None
              Right = None }
        | val1 when Map.containsKey val1 constsMap -> 
            { Value = 0
              Operation = Map.find val1 constsMap
              Left = None
              Right = None }
        | val1 when isNumber val1 &temp ->
            { Value = temp
              Operation = '\u0000'
              Right = None
              Left = None }
        | val1 when r = val1.Length - 1 && val1[r] = '|' && l = 0 && val1[0] = '|' ->
            { Value = 0
              Operation = '|'
              Right = Some(convert (val1[1 .. val1.Length - 2]))
              Left = None }
        | val1 when r = val1.Length - 1 && l > 0 ->
            let sub = line[..l - 1]
            let result = Map.tryFind sub funcsMap

            match result with
            | Some x ->
                { Value = 0
                  Operation = x
                  Left = None
                  Right = Some(convert (val1[l + 1 .. r - 1])) }
            | _ ->
                let (lft, rght, i) =
                    searchSymbol (breakLine l r operations[0] removed) removed l r 0

                { Value = 0
                  Operation = operations[i]
                  Left = Some(convert (lft))
                  Right = Some(convert (rght)) }
        | _ ->
            let (lft, rght, i) =
                searchSymbol (breakLine l r operations[0] removed) removed l r 0

            { Value = 0
              Operation = operations[i]
              Left = Some(convert (lft))
              Right = Some(convert (rght)) }

    convert (line.Replace(" ", ""))

let calculateFunc (node: Node) (args: Map<char, float>): float =
    let rec op (node: Node) : float =
        match node.Operation with
        | '\u0000' -> node.Value    
        | '\u0001' -> Math.PI
        | '\u0002' -> Math.E
        | '+' -> op node.Left.Value + op node.Right.Value
        | '-' -> op node.Left.Value - op node.Right.Value
        | '*' -> op node.Left.Value * op node.Right.Value
        | '/' -> op node.Left.Value / op node.Right.Value
        | '^' -> Math.Pow(op node.Left.Value, op node.Right.Value)
        | '|' -> Math.Abs(op node.Right.Value)
        | '\u0010' -> Math.Log(op node.Right.Value)
        | '\u0011' -> Math.Log10(op node.Right.Value)
        | '\u0012' -> Math.Sin(op node.Right.Value)
        | '\u0013' -> Math.Cos(op node.Right.Value)
        | '\u0014' -> Math.Sqrt(op node.Right.Value)
        | '\u0015' -> Math.Log2(op node.Right.Value)
        | '\u0016' -> Math.Tan(op node.Right.Value)
        | '\u0017' -> 1. / Math.Tan(op node.Right.Value)
        | '\u0018' -> Math.Exp(op node.Right.Value)
        | '\u0019' -> Math.Asin(op node.Right.Value)
        | '\u001A' -> Math.Acos(op node.Right.Value)
        | '\u001B' -> Math.Atan(op node.Right.Value)
        | '\u001C' -> Math.PI / 2. - Math.Atan(op node.Right.Value)
        | '\u001D' -> 
            match (op node.Right.Value) with
            | 0. -> 0.
            | val1 when val1 < 0 -> -1.
            | _ -> 1
        | val1 when isArg node ->
            match args.TryFind val1 with
            | Some arg -> arg
            | _ -> raise (ArgumentNotExist (string val1))
        | _ -> raise (UnknownOperation (string node.Operation))

    op node
