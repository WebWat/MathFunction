module Parser

open Node
open System
open System.Globalization

exception ArgumentNotExist of string
exception InvalidBracket of string
exception InvalidModule of string

let isNumber (number: string) (temp: outref<float>) =
    // Parse with a dot
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

// Get the index of the outer bracket or module.
let private getClosest (rightBracket: int) (leftModule: int) =
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
let rec private findBracketsIndexes (line: string) (total: int) (current: int) (op: Operation) =
    if total = 0 then
        current - int op
    elif current = -1 then
        raise (InvalidBracket line)
    elif line[current] = '(' then
        findBracketsIndexes line (total - 1) (current + int op) op
    elif line[current] = ')' then
        findBracketsIndexes line (total + 1) (current + int op) op
    else
        findBracketsIndexes line total (current + int op) op

// Returns the first outer modules indexes.
let rec private findModulesIndexes (line: string) (total: int) (current: int) (lastClose: bool) (op: Operation) =
    if total = 0 then
        current - int op
    elif current = line.Length - 1 || current = 0 then
        current
    elif current >= line.Length then
        raise (InvalidModule line)
    elif line[current] = '|' then
        if
            not lastClose
            && line[current - 1] <> ')'
            && Array.contains line[current - 1] SYMBOLS
        then
            findModulesIndexes line (total + 1) (current + int op) false op
        else
            findModulesIndexes line (total - 1) (current + int op) true op
    else
        findModulesIndexes line total (current + int op) false op

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
let getBracketsIndexesRightToLeft (line: string) =
    let (left, operation) = getClosest (line.IndexOf '(') (line.IndexOf '|')

    match operation with
    | ')' -> (left, findBracketsIndexes line -1 (left + 1) Operation.Inc)
    | '|' -> (left, findModulesIndexes line 1 (left + 1) false Operation.Inc)
    | _ -> (-1, -1)

let getBracketsIndexesLeftToRight (line: string) =
    let (right, operation) = getClosest (line.LastIndexOf ')') (line.LastIndexOf '|')

    match operation with
    | ')' -> (findBracketsIndexes line 1 (right - 1) Operation.Dec, right)
    | '|' -> (findModulesIndexes line -1 (right - 1) true Operation.Dec, right)
    | _ -> (-1, -1)

// Gets the character index, after passing various checks.
// ****************

// Only for '^' operation.
let getSymbolIndexRightToLeft (leftBracket: int) (rightBracket: int) (symbol: char) (line: string) =
    lookOutside line symbol leftBracket rightBracket 0 Operation.Inc

let getSymbolIndexLeftToRight (leftBracket: int) (rightBracket: int) (symbol: char) (line: string) =
    // Check that the symbol is not in other brackets
    let rec checkLeftSide (line: string) (lLast: int) index =
        let subLine = line[.. lLast - 2]

        let (left, right) = getBracketsIndexesLeftToRight subLine

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
// ****************

// Removes all unnecessary brackets.
let rec removeExtraBrackets (line: string) =
    let args = getBracketsIndexesLeftToRight line

    if line[0] = '(' && args = (0, line.Length - 1) then
        removeExtraBrackets line[1 .. line.Length - 2]
    else
        (line, args)

// Splitting a string by operations.
let rec breakLine (lbracket: int) (rbracket: int) (symbol: char) (line: string) =
    let index =
        if symbol = '^' then
            let (l, r) = getBracketsIndexesRightToLeft line
            getSymbolIndexRightToLeft l r symbol line
        else
            getSymbolIndexLeftToRight lbracket rbracket symbol line
    
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
        if item + 1 = OPERATIONS.Length then
            raise (UnknownOperation line)
        else
            searchSymbol 
                (breakLine leftBracket rightBracket (OPERATIONS[item + 1]) line) 
                line 
                leftBracket 
                rightBracket 
                (item + 1)
    | (val1, val2) -> (val1, val2, item)


// Converts a function represented in a string into a recursive Node entry.
let convertToFunction (line: string) : Node =
    let mutable temp = 0.

    let rec convert (line: string) =
        // Removing extra brackets
        let (removed, (l, r)) = removeExtraBrackets line

        //printfn "%s  %d %d " removed l r

        match removed with
        | val1 when 
            val1.Length = 1 && Array.contains (char val1) ALLOWED_ARGS
            ->
            { Value = 0
              Operation = (char val1)
              Left = None
              Right = None }
        | val1 when Map.containsKey val1 CONSTS_MAP -> 
            { Value = 0
              Operation = Map.find val1 CONSTS_MAP
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
            let result = Map.tryFind sub FUNCS_MAP

            match result with
            | Some x ->
                { Value = 0
                  Operation = x
                  Left = None
                  Right = Some(convert (val1[l + 1 .. r - 1])) }
            | _ ->
                
                let (lft, rght, i) =
                    searchSymbol (breakLine l r OPERATIONS[0] removed) removed l r 0

                { Value = 0
                  Operation = OPERATIONS[i]
                  Left = Some(convert (lft))
                  Right = Some(convert (rght)) }
        | _ ->
            let (lft, rght, i) =
                searchSymbol (breakLine l r OPERATIONS[0] removed) removed l r 0

            { Value = 0
              Operation = OPERATIONS[i]
              Left = Some(convert (lft))
              Right = Some(convert (rght)) }

    convert (line.Replace(" ", ""))

// Function result calculation.
let calculateFunction (node: Node) (args: Map<char, float>): float =
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

        // Look at FUNCS_MAP in Node.fs for information.
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