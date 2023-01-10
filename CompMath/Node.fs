module Node

open System
open System.Globalization
open System.Text.RegularExpressions

let funcs =
    [| "ln"
       "lg"
       "sin"
       "cos"
       "sqrt"
       "log2"
       "tg"
       "ctg"
       "exp"
       "arcsin"
       "arccos"
       "arctg"
       "arcctg"
       "sh"
       "ch"
       "th"
       "cth"
       "sch"
       "csch" |]

let consts = [| "pi"; "e"; "" |]

let symbols = [| '+'; '-'; '*'; '/'; '^' |]

type Node =
    { Value: Option<float>
      Operation: string
      Left: Option<Node>
      Right: Option<Node> }

    override this.ToString() =
        let needBrackets (node: Node) =
            node.Operation <> "x"
            && node.Operation <> "|"
            && node.Operation <> "^"
            && not (Array.contains node.Operation consts)
            && not (Array.contains node.Operation funcs)

        let rec convert2str node =
            match node.Operation with
            | "" ->
                if node.Value.Value < 0. then
                    $"({node.Value.Value})"
                else
                    string node.Value.Value
            | "x" -> "x"
            | "pi" -> "pi"
            | "e" -> "e"
            | "+" -> $"{convert2str node.Left.Value}+{convert2str node.Right.Value}"
            | "|" -> $"|{convert2str node.Right.Value}|"
            | "-" ->
                if node.Left.Value.Value.IsSome && node.Left.Value.Value.Value = 0 then
                    if
                        needBrackets node.Right.Value
                        &&
                        // If meet the operation of division or multiplication -> do not put brackets
                        node.Right.Value.Operation <> "/"
                        && node.Right.Value.Operation <> "*"
                    then
                        $"-({convert2str node.Right.Value})"
                    else
                        $"-{convert2str node.Right.Value}"
                else if
                    needBrackets node.Right.Value
                    && node.Right.Value.Operation <> "/"
                    && node.Right.Value.Operation <> "*"
                then
                    $"{convert2str node.Left.Value}-({convert2str node.Right.Value})"
                else
                    $"{convert2str node.Left.Value}-{convert2str node.Right.Value}"
            | "*" ->
                if
                    needBrackets node.Left.Value
                    && node.Left.Value.Operation <> "/"
                    && node.Left.Value.Operation <> "*"
                    && needBrackets node.Right.Value
                    && node.Right.Value.Operation <> "/"
                    && node.Right.Value.Operation <> "*"
                then
                    $"({convert2str node.Left.Value})*({convert2str node.Right.Value})"
                elif
                    needBrackets node.Left.Value
                    && node.Left.Value.Operation <> "/"
                    && node.Left.Value.Operation <> "*"
                then
                    $"({convert2str node.Left.Value})*{convert2str node.Right.Value}"
                elif
                    needBrackets node.Right.Value
                    && node.Right.Value.Operation <> "/"
                    && node.Right.Value.Operation <> "*"
                then
                    $"{convert2str node.Left.Value}*({convert2str node.Right.Value})"
                else
                    $"{convert2str node.Left.Value}*{convert2str node.Right.Value}"
            | "/" ->
                if
                    needBrackets node.Left.Value
                    && node.Left.Value.Operation <> "/"
                    && needBrackets node.Right.Value
                    && node.Left.Value.Operation <> "*"
                then
                    $"({convert2str node.Left.Value})/({convert2str node.Right.Value})"
                elif
                    needBrackets node.Left.Value
                    && node.Left.Value.Operation <> "/"
                    && node.Left.Value.Operation <> "*"
                then
                    $"({convert2str node.Left.Value})/{convert2str node.Right.Value}"
                elif needBrackets node.Right.Value then
                    $"{convert2str node.Left.Value}/({convert2str node.Right.Value})"
                else
                    $"{convert2str node.Left.Value}/{convert2str node.Right.Value}"
            | "^" ->
                if needBrackets node.Left.Value && needBrackets node.Right.Value then
                    $"({convert2str node.Left.Value})^({convert2str node.Right.Value})"
                elif needBrackets node.Left.Value then
                    $"({convert2str node.Left.Value})^{convert2str node.Right.Value}"
                elif needBrackets node.Right.Value then
                    $"{convert2str node.Left.Value}^({convert2str node.Right.Value})"
                else
                    $"{convert2str node.Left.Value}^{convert2str node.Right.Value}"
            | val1 when Array.exists (fun x -> val1 = x) funcs ->
                $"{Array.find (fun x -> val1 = x) funcs}({convert2str node.Right.Value})"
            | _ -> failwith "[toString] Unknown function, operation, or constant!"

        convert2str this

type Operation =
    | Dec = -1
    | Inc = 1

let isComplexFunction (node: Node) = Array.contains node.Operation funcs

let isConst (node: Node) = Array.contains node.Operation consts

let isNumber (number: string) =
    let mutable temp = 0.0
    // Parse with a dot
    Double.TryParse(number.ToString().AsSpan(), NumberStyles.Any, CultureInfo.InvariantCulture, &temp)

// Get the index of the nearest bracket or module.
let private getClosest rightBracket leftModule =
    if
        rightBracket <> -1
        && (leftModule = -1 || leftModule <> -1 && rightBracket > leftModule)
    then
        (rightBracket, ')')
    elif
        leftModule <> -1
        && (rightBracket = -1 || rightBracket <> -1 && leftModule > rightBracket)
    then
        (leftModule, '|')
    else
        (-1, '-')

// Returns the first outer brackets indexes.
let rec private findBracketsIndexes (line: string) (total: int) (current: int) (inc: Operation) =
    if total = 0 then
        current - int inc
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
    elif line[current] = '|' then
        if
            not lastClose
            && line[current - 1] <> ')'
            && Regex.IsMatch(string line[current - 1], @"\W")
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

    if (leftBracket, rightBracket) = (-1, -1) then
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
let getSymbolIndexRight2Left (leftBracket: int) (rightBracket: int) (symbol: char) (line: string) =
    // Check that the symbol is not in other brackets
    let rec checkRightSide (line: string) (rLast: int) (rCurrent: int) index =
        let subLine = line[rLast + 2 ..]
        let (left, right) = getBracketsIndexesRight2Left subLine

        if left = -1 || right = -1 || (rLast < index && index < left + rLast + 2) then
            if index = -1 then -1 else rCurrent + 1
        else
            checkRightSide subLine right (right + rCurrent + 2) (lookOutside subLine symbol left right 0 Operation.Inc)

    let index = lookOutside line symbol leftBracket rightBracket 0 Operation.Inc

    // If found symbol beyond expression
    if rightBracket <> -1 && index > rightBracket + 1 then
        checkRightSide line rightBracket rightBracket index
    else
        index

let getSymbolIndexLeft2Right (leftBracket: int) (rightBracket: int) (symbol: char) (line: string) =
    // Check that the symbol is not in other brackets
    let rec checkLeftSide (line: string) (lLast: int) index =
        let subLine = line[.. lLast - 2]
        let (left, right) = getBracketsIndexesLeft2Right subLine

        if left = -1 || right = -1 || (right < index && index < lLast) then
            index
        else
            checkLeftSide subLine left (lookOutside subLine symbol left right (subLine.Length - 1) Operation.Dec)

    let index =
        lookOutside line symbol leftBracket rightBracket (line.Length - 1) Operation.Dec

    // If found symbol beyond expression
    if leftBracket <> -1 && index < leftBracket + 1 then
        checkLeftSide line leftBracket index
    else
        index

// Removes all unnecessary brackets
let rec removeExtraBrackets (line: string) =
    let args = getBracketsIndexesLeft2Right line

    if line[0] = '(' && args = (0, line.Length - 1) then
        removeExtraBrackets line[1 .. line.Length - 2]
    else
        (new string (line), args)

// Splitting a string by operations.
let rec breakLine (brackets: int * int) (symbol: char) (line: string) =
    let (lbracket, rbracket) = brackets

    let index =
        if symbol = '^' then
            let (l, r) = getBracketsIndexesRight2Left line
            getSymbolIndexRight2Left l r symbol line
        else
            getSymbolIndexLeft2Right lbracket rbracket symbol line

    let comp = lazy (line[ rbracket .. rbracket + 1 ].LastIndexOf symbol)

    // If symbol not found
    if index = -1 then
        ("-", "-")
    // If -number
    elif symbol = '-' && index = 0 then
        ("0", line[1..])
    // If multiply, divide and pow (right side only) ???
    elif
        lbracket <> -1
        && lbracket < index
        && symbol <> '+'
        && symbol <> '-'
        && comp.Force() <> -1
    then
        (line[0..rbracket], line[rbracket + 2 ..])
    // All other operations
    else
        (line[.. index - 1], line[index + 1 ..])

// Looking for symbols which can split the function.
let rec searchSymbol (operands: string * string) (line: string) (brackets: int * int) (item: int) =
    match operands with
    | ("-", "-") ->
        if item + 1 = symbols.Length then
            failwith "Unknow operation"
        else
            searchSymbol (breakLine brackets (symbols[item + 1]) line) line brackets (item + 1)
    | (val1, val2) -> (val1, val2, item)


// Converts a function represented in a string into a recursive Node entry.
let convertToFunc (line: string) : Node =
    let rec convert (line: string) =
        // Removing extra brackets
        let (removed, (l, r)) = removeExtraBrackets line

        //printfn "l: %d r: %d => %s" l r removed

        match removed with
        | "x" ->
            { Value = None
              Operation = "x"
              Left = None
              Right = None }
        | "pi" ->
            { Value = None
              Operation = "pi"
              Left = None
              Right = None }
        | "e" ->
            { Value = None
              Operation = "e"
              Left = None
              Right = None }
        | val1 when isNumber (val1) ->
            { Value = Some(float val1)
              Operation = ""
              Right = None
              Left = None }
        | val1 when r = val1.Length - 1 && val1[r] = '|' && l = 0 && val1[0] = '|' ->
            { Value = None
              Operation = "|"
              Right = Some(convert (val1[1 .. val1.Length - 2]))
              Left = None }
        | val1 when r = val1.Length - 1 && l > 0 ->
            let result = Array.tryFind (fun (x: string) -> val1.StartsWith(x)) funcs

            match result with
            | Some x when x.Length = l ->
                { Value = None
                  Operation = x
                  Left = None
                  Right = Some(convert (val1[l + 1 .. r - 1])) }
            | _ ->
                let (lft, rght, i) =
                    searchSymbol (breakLine (l, r) symbols[0] removed) removed (l, r) 0

                { Value = None
                  Operation = string symbols[i]
                  Left = Some(convert (lft))
                  Right = Some(convert (rght)) }

        | _ ->
            let (lft, rght, i) =
                searchSymbol (breakLine (l, r) symbols[0] removed) removed (l, r) 0

            { Value = None
              Operation = string symbols[i]
              Left = Some(convert (lft))
              Right = Some(convert (rght)) }

    convert (line.Replace(" ", ""))

let rec calculateFunc (node: Node) (x: float) : float =
    match node.Operation with
    | "" -> node.Value.Value
    | "x" -> x
    | "pi" -> Math.PI
    | "e" -> Math.E
    | "+" -> calculateFunc node.Left.Value x + calculateFunc node.Right.Value x
    | "-" -> calculateFunc node.Left.Value x - calculateFunc node.Right.Value x
    | "*" -> calculateFunc node.Left.Value x * calculateFunc node.Right.Value x
    | "/" -> calculateFunc node.Left.Value x / calculateFunc node.Right.Value x
    | "^" -> Math.Pow(calculateFunc node.Left.Value x, calculateFunc node.Right.Value x)
    | "|" -> Math.Abs(calculateFunc node.Right.Value x)
    | "ln" -> Math.Log(calculateFunc node.Right.Value x)
    | "lg" -> Math.Log10(calculateFunc node.Right.Value x)
    | "sin" -> Math.Sin(calculateFunc node.Right.Value x)
    | "cos" -> Math.Cos(calculateFunc node.Right.Value x)
    | "sqrt" -> Math.Sqrt(calculateFunc node.Right.Value x)
    | "log2" -> Math.Log2(calculateFunc node.Right.Value x)
    | "tg" -> Math.Tan(calculateFunc node.Right.Value x)
    | "ctg" -> 1. / Math.Tan(calculateFunc node.Right.Value x)
    | "exp" -> Math.Exp(calculateFunc node.Right.Value x)
    | "arcsin" -> Math.Asin(calculateFunc node.Right.Value x)
    | "arccos" -> Math.Acos(calculateFunc node.Right.Value x)
    | "arctg" -> Math.Atan(calculateFunc node.Right.Value x)
    | "arcctg" -> Math.PI / 2. - Math.Atan(calculateFunc node.Right.Value x)
    | "sh" -> Math.Sinh(calculateFunc node.Right.Value x)
    | "ch" -> Math.Cosh(calculateFunc node.Right.Value x)
    | "th" -> Math.Tanh(calculateFunc node.Right.Value x)
    | "cth" -> 1. / Math.Tanh(calculateFunc node.Right.Value x)
    | "sch" -> 1. / Math.Cosh(calculateFunc node.Right.Value x)
    | "csch" -> 1. / Math.Sinh(calculateFunc node.Right.Value x)
    | _ -> failwith "lol"
