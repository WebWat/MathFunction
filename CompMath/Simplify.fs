module Simplify

open Node
open System
open System.Text

let openBrackets (node: Node) =
    let mutable clear = true

    let op (node: Node) =
        let func = node.ToString()
        let ind = func.IndexOf("-(")

        if ind = -1 then
            node
        else
            clear <- false

            let inner = func[ind + 1 ..]

            let mutable (l, r) = getBracketsIndexesRight2Left inner
            let rfirst = r

            let temp = inner[ ..r ].ToCharArray()

            let mutable brackets: (int * int) list = []

            while l <> -1 do
                temp[l] <- '`'
                temp[r] <- '`'

                let (l1, r1) = getBracketsIndexesLeft2Right (new String(temp))

                if l1 = -1 || (r <> rfirst && r1 < r) then
                    l <- -1
                else
                    r <- r1
                    l <- l1
                    brackets <- List.append brackets [ (l, r) ]

            let builder = new StringBuilder(func[..ind])

            let mutable item = 0
            let mutable i = 1

            while i < rfirst do
                if brackets.Length <> 0 && fst brackets[item] = i then
                    builder.Append inner[fst brackets[item] .. snd brackets[item]] |> ignore
                    i <- snd brackets[item]
                    if item + 1 <> brackets.Length then item <- item + 1 else ()
                elif temp[i] = '+' then
                    builder.Append '-' |> ignore
                elif temp[i] = '-' then
                    builder.Append '+' |> ignore
                else
                    builder.Append temp[i] |> ignore

                i <- i + 1

            builder.Append(inner[rfirst + 1 ..]) |> ignore

            convertToFunc (builder.ToString())

    let rec clearFunc (result: Node) =
        if clear then
            result
        else
            clear <- true
            clearFunc (op result)

    clearFunc (op node)

// (x*x)^4
// x^x

let simplifyMultiply (node: Node) =
    let (|Multiply|Minus|Another|) (node: Node) =
        if node.Left.IsSome && node.Right.IsSome && node.Operation = "*" then
            Multiply
        elif node.Left.IsSome && node.Right.IsSome && node.Operation = "-" then
            Minus
        else
            Another

    let (|Both|Left|Right|None|) (node: Node) =
        if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
            Both
        elif node.Left.Value.Value.IsSome then
            Left
        elif node.Right.Value.Value.IsSome then
            Right
        else
            None

    let mutable x0 = 1.
    let mutable funcList: Node list = []

    let rec multiplyNumbers (node: Node) : Node =
        match node with
        | Multiply ->
            if x0 = 0. then
                node
            else
                match node with
                | Both ->
                    x0 <- x0 * node.Left.Value.Value.Value * node.Right.Value.Value.Value

                    { Value = Some(x0)
                      Operation = ""
                      Left = None
                      Right = None }
                | Left ->
                    x0 <- x0 * node.Left.Value.Value.Value
                    multiplyNumbers node.Right.Value
                | Right ->
                    x0 <- x0 * node.Right.Value.Value.Value
                    multiplyNumbers node.Left.Value
                | None ->
                    { Value = None
                      Operation = node.Operation
                      Left = Some(multiplyNumbers node.Left.Value)
                      Right = Some(multiplyNumbers node.Right.Value) }
        | Minus ->
            if node.Right.Value.Value.IsSome then
                x0 <- x0 * -node.Right.Value.Value.Value

                { Value = Some(-node.Right.Value.Value.Value)
                  Operation = ""
                  Left = None
                  Right = None }
            else
                x0 <- -x0
                multiplyNumbers node.Right.Value
        | Another ->
            funcList <- List.append funcList [ node ]
            node

    let rec clearNumbers (node: Node) : Node =
        match node with
        | Multiply ->
            match node with
            | Both ->
                { Value = Some(0.)
                  Operation = ""
                  Left = None
                  Right = None }
            | Left -> clearNumbers node.Right.Value
            | Right -> clearNumbers node.Left.Value
            | None ->
                { Value = None
                  Operation = node.Operation
                  Left = Some(clearNumbers node.Left.Value)
                  Right = Some(clearNumbers node.Right.Value) }
        | Minus
        | Another -> node

    let splitFunc (node: Node) : (string * float) =
        let mutable coef = 1.

        let rec getWithoutCoef (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "^") ->
                if node.Right.Value.Value.IsSome then
                    coef <- node.Right.Value.Value.Value
                    node.Left.Value
                else
                    { Value = None
                      Operation = node.Operation
                      Left = Some(getWithoutCoef node.Left.Value)
                      Right = Some(getWithoutCoef node.Right.Value) }
            | _ -> node

        let rec splitFuncs (node: Node) : string list =
            match node with
            | Multiply ->
                match (node.Left.Value.Operation, node.Right.Value.Operation) with
                | (val1, val2) when val1 <> "*" && val2 <> "*" ->
                    [ node.Left.Value.ToString(); node.Right.Value.ToString() ]
                | (_, "*") -> List.append [ node.Left.Value.ToString() ] (splitFuncs node.Right.Value)
                | ("*", _) -> List.append [ node.Right.Value.ToString() ] (splitFuncs node.Left.Value)
                | _ -> List.append (splitFuncs node.Left.Value) (splitFuncs node.Right.Value)
            | Minus
            | Another -> []

        let clearedNode = getWithoutCoef node
        let funcs = splitFuncs clearedNode

        if funcs.Length = 0 then
            (clearedNode.ToString(), coef)
        else
            let degree =
                List.filter (fun x -> isNumber x) funcs
                |> List.map (fun x -> Math.Pow((float x), coef))

            let degreeCheck =
                if degree.Length = 0 then
                    1.
                else
                    degree |> List.reduce (fun a b -> a * b)

            let result =
                String.Join('*', List.filter (fun x -> not (isNumber x)) funcs |> List.sort)

            x0 <- x0 * degreeCheck

            (result, coef)

    let clearFuncs (node: Node) =
        if funcList.Length = 0 then
            node
        else
            let arr = List.map (fun x -> splitFunc x) funcList
            printfn "converted: %A" arr

            let rec go (arr: (string * float) list) (item: int) =
                if item = arr.Length then
                    arr
                else
                    let same = List.filter (fun x -> fst arr[item] = fst x) arr
                    let notsame = List.filter (fun x -> fst arr[item] <> fst x) arr
                    let sum = (fst same[0], List.sumBy (fun x -> snd x) same)

                    if snd sum <> 0 then
                        go (List.append [ sum ] notsame) (item + 1)
                    else
                        go notsame (item)

            let result = go arr 0
            printfn "arr: %A" result

            if result.Length = 0 then
                { Value = Some(1.)
                  Operation = ""
                  Left = None
                  Right = None }
            else
                let func =
                    String.Join(
                        '*',
                        (List.map (fun x -> if snd x = 1. then $"{fst x}" else $"({fst x})^({snd x})") result)
                    )

                convertToFunc func

    let data = multiplyNumbers node

    match x0 with
    | 0. ->
        { Value = Some(0.)
          Operation = ""
          Left = None
          Right = None }
    | _ ->
        let result = clearNumbers data |> clearFuncs

        printfn "x0: %f" x0

        if x0 <> 1 && not (isConst result) then
            { Value = None
              Operation = "*"
              Left =
                Some(
                    { Value = Some(x0)
                      Operation = ""
                      Left = None
                      Right = None }
                )
              Right = Some(result) }
        else
            result

let simplifySum (node: Node) =
    let mutable x0 = 0.
    let mutable funcList: Node list = []

    let changeIfNeg (op: string) (number: float) = if op = "-" then -number else number

    let changeFuncIfNeg (op: string) (node: Node) =
        if op = "-" then
            { Value = None
              Operation = "-"
              Left =
                Some(
                    { Value = Some(0.)
                      Operation = ""
                      Left = None
                      Right = None }
                )
              Right = Some(node) }
        else
            node

    let rec sumNumbers (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" ->
            if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
                let sum =
                    node.Left.Value.Value.Value
                    + changeIfNeg node.Operation node.Right.Value.Value.Value

                x0 <- x0 + sum

                { Value = Some(0.)
                  Operation = ""
                  Left = None
                  Right = None }
            // -x = 0 - x
            elif node.Left.Value.Value.IsSome && node.Operation = "-" then
                x0 <- x0 + node.Left.Value.Value.Value

                { Value = None
                  Operation = node.Operation
                  Left =
                    Some(
                        { Value = Some(0.)
                          Operation = ""
                          Left = None
                          Right = None }
                    )
                  Right = Some(sumNumbers node.Right.Value) }
            elif node.Left.Value.Value.IsSome then
                x0 <- x0 + node.Left.Value.Value.Value

                sumNumbers
                    { Value = None
                      Operation = node.Right.Value.Operation
                      Left = node.Right.Value.Left
                      Right = node.Right.Value.Right }
            elif node.Right.Value.Value.IsSome then
                x0 <- x0 + changeIfNeg node.Operation node.Right.Value.Value.Value

                sumNumbers
                    { Value = None
                      Operation = node.Left.Value.Operation
                      Left = node.Left.Value.Left
                      Right = node.Left.Value.Right }
            else
                { Value = None
                  Operation = node.Operation
                  Left = Some(sumNumbers node.Left.Value)
                  Right = Some(sumNumbers node.Right.Value) }
        | _ -> node

    let rec clearNumbers (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" ->
            if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
                { Value = Some(0.)
                  Operation = ""
                  Left = None
                  Right = None }
            elif
                node.Left.Value.Value.IsSome
                && node.Left.Value.Value.Value <> 0
                && node.Operation <> "-"
            then
                clearNumbers
                    { Value = None
                      Operation = node.Right.Value.Operation
                      Left = node.Right.Value.Left
                      Right = node.Right.Value.Right }
            elif node.Right.Value.Value.IsSome then
                clearNumbers
                    { Value = None
                      Operation = node.Left.Value.Operation
                      Left = node.Left.Value.Left
                      Right = node.Left.Value.Right }
            else
                { Value = None
                  Operation = node.Operation
                  Left = Some(clearNumbers node.Left.Value)
                  Right = Some(clearNumbers node.Right.Value) }
        | _ -> node

    let rec recClearNumbers (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            recClearNumbers result (clearNumbers result)

    let rec sumFunctions (node: Node) =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" ->
            if
                (node.Left.Value.Operation = "*"
                 || node.Left.Value.Operation = "^"
                 || node.Left.Value.Operation = "x"
                 || isComplexFunction node.Left.Value)
                && (node.Right.Value.Operation = "*"
                    || node.Right.Value.Operation = "^"
                    || node.Right.Value.Operation = "x"
                    || isComplexFunction node.Right.Value)
            then
                funcList <-
                    List.append funcList [ changeFuncIfNeg val1 node.Right.Value ]
                    |> List.append [ node.Left.Value ]
            elif
                node.Left.Value.Operation = "*"
                || node.Left.Value.Operation = "^"
                || node.Left.Value.Operation = "x"
                || isComplexFunction node.Left.Value
            then
                funcList <- List.append funcList [ node.Left.Value ]
                sumFunctions node.Right.Value
            elif
                node.Right.Value.Operation = "*"
                || node.Right.Value.Operation = "^"
                || node.Right.Value.Operation = "x"
                || isComplexFunction node.Right.Value
            then
                funcList <- List.append funcList [ changeFuncIfNeg val1 node.Right.Value ]
                sumFunctions node.Left.Value
            else
                sumFunctions node.Left.Value
                sumFunctions node.Right.Value
        | _ -> ()

    let rec mulFunction (node: Node) =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "*" || val1 = "^" -> simplifyMultiply node
        | (true, true, val1) when val1 = "+" || val1 = "-" ->
            if
                not (isConst node.Left.Value)
                && node.Left.Value.Operation <> "+"
                && node.Left.Value.Operation <> "+"
                && not (isConst node.Left.Value)
                && node.Right.Value.Operation <> "+"
                && node.Right.Value.Operation <> "+"
            then
                { Value = None
                  Operation = node.Operation
                  Left = Some(simplifyMultiply node.Left.Value)
                  Right = Some(simplifyMultiply node.Right.Value) }
            elif
                not (isConst node.Left.Value)
                && node.Left.Value.Operation <> "+"
                && node.Left.Value.Operation <> "+"
            then
                { Value = None
                  Operation = node.Operation
                  Left = Some(simplifyMultiply node.Left.Value)
                  Right = Some(mulFunction node.Right.Value) }
            elif
                not (isConst node.Right.Value)
                && node.Right.Value.Operation <> "+"
                && node.Right.Value.Operation <> "+"
            then
                { Value = None
                  Operation = node.Operation
                  Left = Some(mulFunction node.Left.Value)
                  Right = Some(simplifyMultiply node.Right.Value) }
            else
                { Value = None
                  Operation = node.Operation
                  Left = Some(mulFunction node.Left.Value)
                  Right = Some(mulFunction node.Right.Value) }

        | _ -> node

    let split (node: Node) : float * string =
        let mutable coef = 1.
        let mutable funcs: string list = []

        let rec splitFuncs (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "*") ->
                if node.Left.Value.Operation <> "*" && node.Right.Value.Operation <> "*" then
                    funcs <- List.append funcs [ node.Left.Value.ToString() ]
                    funcs <- List.append funcs [ node.Right.Value.ToString() ]
                elif node.Left.Value.Operation <> "*" then
                    funcs <- List.append funcs [ node.Left.Value.ToString() ]
                    splitFuncs node.Right.Value
                elif node.Right.Value.Operation <> "*" then
                    funcs <- List.append funcs [ node.Right.Value.ToString() ]
                    splitFuncs node.Left.Value
                else
                    splitFuncs node.Left.Value
                    splitFuncs node.Right.Value
            | _ -> ()

        let rec getWithoutCoef (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "-") ->
                coef <- -coef
                getWithoutCoef node.Right.Value
            | (true, true, "*") ->
                if node.Left.Value.Value.IsSome then
                    coef <- node.Left.Value.Value.Value
                    node.Right.Value
                elif node.Right.Value.Value.IsSome then
                    coef <- node.Right.Value.Value.Value
                    node.Left.Value
                else
                    { Value = None
                      Operation = node.Operation
                      Left = Some(getWithoutCoef node.Left.Value)
                      Right = Some(getWithoutCoef node.Right.Value) }
            | _ -> node

        let nod1 = getWithoutCoef node
        splitFuncs nod1


        if funcs.Length = 0 then
            (coef, nod1.ToString())
        else
            let nod = String.Join('*', List.sort funcs)
            (coef, nod)

    let clearSumFunctions (node: Node) =
        sumFunctions node

        printfn "sumFunc: %A" (List.map (fun x -> x.ToString()) funcList)

        if funcList.Length = 0 then
            node
        else
            let arr = List.map (fun x -> split x) funcList
            printfn "converted: %A" arr

            let rec go (arr: (float * string) list) (item: int) =
                if item = arr.Length then
                    arr
                else
                    let same = List.filter (fun x -> snd arr[item] = snd x) arr
                    let notsame = List.filter (fun x -> snd arr[item] <> snd x) arr
                    let sum = (List.sumBy (fun x -> fst x) same, snd same[0])

                    if fst sum <> 0 then
                        go (List.append [ sum ] notsame) (item + 1)
                    else
                        go notsame (item)

            let data = go arr 0
            printfn "arr: %A" data

            let func =
                String.Join('+', (List.map (fun x -> if fst x = 1. then $"{snd x}" else $"({fst x})*({snd x})") data))

            convertToFunc func

    let pre = mulFunction node |> sumNumbers
    printfn "pre: %s" (pre.ToString())
    let result = recClearNumbers pre (clearNumbers pre)
    printfn "result: %s" (result.ToString())
    let data = clearSumFunctions result

    printfn "data: %s" (data.ToString())
    printfn "nt: %f" x0

    if x0 <> 0 then
        { Value = None
          Operation = "+"
          Left = Some(data)
          Right =
            Some(
                { Value = Some(x0)
                  Operation = ""
                  Left = None
                  Right = None }
            ) }
    else
        data

let expandFunc (node: Node) =
    let rec multiplyBy (multiplier: Node) (right: Node) =
        match (right.Left.IsSome, right.Right.IsSome, right.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" ->
            if
                (right.Left.Value.Operation <> "+" && right.Left.Value.Operation <> "-")
                && (right.Right.Value.Operation <> "+" && right.Right.Value.Operation <> "-")
            then
                { Value = None
                  Operation = val1
                  Left =
                    Some(
                        { Value = None
                          Operation = "*"
                          Left = Some(multiplier)
                          Right = right.Left }
                    )
                  Right =
                    Some(
                        { Value = None
                          Operation = "*"
                          Left = Some(multiplier)
                          Right = right.Right }
                    ) }
            elif (right.Left.Value.Operation <> "+" && right.Left.Value.Operation <> "-") then
                { Value = None
                  Operation = val1
                  Left =
                    Some(
                        { Value = None
                          Operation = "*"
                          Left = Some(multiplier)
                          Right = right.Left }
                    )
                  Right = Some(multiplyBy multiplier right.Right.Value) }
            elif (right.Right.Value.Operation <> "+" && right.Right.Value.Operation <> "-") then
                { Value = None
                  Operation = val1
                  Left = Some(multiplyBy multiplier right.Left.Value)
                  Right =
                    Some(
                        { Value = None
                          Operation = "*"
                          Left = Some(multiplier)
                          Right = right.Right }
                    ) }
            else
                { Value = None
                  Operation = val1
                  Left = Some(multiplyBy multiplier right.Left.Value)
                  Right = Some(multiplyBy multiplier right.Right.Value) }
        | _ -> failwith "what???"

    let rec operation (left: Node) (right: Node) =
        match (left.Left.IsSome, left.Right.IsSome, left.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" ->
            if
                (left.Left.Value.Operation <> "+" && left.Left.Value.Operation <> "-")
                && (left.Right.Value.Operation <> "+" && left.Right.Value.Operation <> "-")
            then
                { Value = None
                  Operation = val1
                  Left = Some(multiplyBy left.Left.Value right)
                  Right = Some(multiplyBy left.Right.Value right) }
            elif (left.Left.Value.Operation <> "+" && left.Left.Value.Operation <> "-") then
                { Value = None
                  Operation = val1
                  Left = Some(multiplyBy left.Left.Value right)
                  Right = Some(operation left.Right.Value right) }
            elif (left.Right.Value.Operation <> "+" && left.Right.Value.Operation <> "-") then
                { Value = None
                  Operation = val1
                  Left = Some(operation left.Left.Value right)
                  Right = Some(multiplyBy left.Right.Value right) }
            else
                { Value = None
                  Operation = val1
                  Left = Some(operation left.Left.Value right)
                  Right = Some(operation left.Right.Value right) }
        | _ -> failwith "what???"

    let rec findMul (node: Node) =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, "*") ->
            if
                (node.Left.Value.Operation = "+" || node.Left.Value.Operation = "-")
                && (node.Right.Value.Operation = "+" || node.Right.Value.Operation = "-")
            then
                operation node.Left.Value node.Right.Value |> simplifySum
            elif
                (node.Left.Value.Operation = "+" || node.Left.Value.Operation = "-")
                && (isNotComplexExpression node.Right.Value)
            then
                multiplyBy node.Right.Value node.Left.Value |> simplifySum
            elif
                (node.Right.Value.Operation = "+" || node.Right.Value.Operation = "-")
                && (isNotComplexExpression node.Left.Value)
            then
                multiplyBy node.Left.Value node.Right.Value |> simplifySum
            else
                { Value = None
                  Operation = "*"
                  Left = Some(findMul node.Left.Value)
                  Right = Some(findMul node.Right.Value) }
        | (true, true, val1) ->
            { Value = None
              Operation = val1
              Left = Some(findMul node.Left.Value)
              Right = Some(findMul node.Right.Value) }
        | _ -> node

    let rec recClear (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            recClear result (openBrackets (findMul result))

    recClear node (openBrackets (findMul node)) |> simplifySum
