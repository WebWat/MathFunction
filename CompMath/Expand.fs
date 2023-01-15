module Expand

open Node
open System
open System.Text

let openBrackets (node: Node) =
    let operation (node: Node) =
        let func = node.ToString()
        let ind = func.IndexOf("-(")

        if ind = -1 then
            node
        else
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

            let builder = new StringBuilder(func[ .. ind - 1])

            let mutable item = 0
            let mutable i = 1

            if func[ind] = '-' && temp[i] = '-' then
                builder.Append("+") |> ignore
            else
                builder.Append(func[ind]) |> ignore

            while i < rfirst do
                if brackets.Length <> 0 && fst brackets[item] = i then
                    builder.Append inner[fst brackets[item] .. snd brackets[item]] |> ignore
                    i <- snd brackets[item]
                    if item + 1 <> brackets.Length then item <- item + 1 else ()
                elif temp[i] = '+' then
                    builder.Append '-' |> ignore
                elif temp[i] = '-' then
                    if i <> 1 then
                        builder.Append '+' |> ignore
                    else
                        ()
                else
                    builder.Append temp[i] |> ignore

                i <- i + 1

            builder.Append(inner[rfirst + 1 ..]) |> ignore

            convertToFunc (builder.ToString())

    let rec clearFunc (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            clearFunc result (operation result)

    clearFunc node (operation node)
       
// recClear?
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
        let mutable sign = 1.

        let rec getWithoutDegree (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "^") ->
                if node.Right.Value.Value.IsSome then
                    coef <- node.Right.Value.Value.Value
                    node.Left.Value
                else
                    { Value = None
                      Operation = node.Operation
                      Left = Some(getWithoutDegree node.Left.Value)
                      Right = Some(getWithoutDegree node.Right.Value) }
            | _ -> node

        let rec splitFuncs (node: Node) : string list =
            match node with
            | Minus -> 
                 sign <- -sign
                 splitFuncs node.Right.Value
            | Multiply ->
                match (node.Left.Value.Operation, node.Right.Value.Operation) with
                | (val1, val2) when val1 <> "*" && val2 <> "*" ->
                    [ node.Left.Value.ToString(); node.Right.Value.ToString() ]
                | (_, "*") -> List.append [ node.Left.Value.ToString() ] (splitFuncs node.Right.Value)
                | ("*", _) -> List.append [ node.Right.Value.ToString() ] (splitFuncs node.Left.Value)
                | _ -> List.append (splitFuncs node.Left.Value) (splitFuncs node.Right.Value)
            | Another -> []

        let clearedNode = getWithoutDegree node
        let funcs = splitFuncs clearedNode

        if funcs.Length = 0 then
            (clearedNode.ToString(), coef)
        else
            let degree = List.filter (fun x -> isNumber x) funcs

            let degreeCheck =
                if degree.Length = 0 then
                    1.
                else
                    Math.Pow( 
                        (degree 
                        |> List.map (fun x -> float x) 
                        |> List.reduce (fun a b -> a * b)) * sign,
                        coef
                    )

            let result =
                String.Join('*', List.filter (fun x -> not (isNumber x)) funcs |> List.sort)

            x0 <- x0 * degreeCheck

            (result, coef)

    let clearFuncs (node: Node) =
        if funcList.Length = 0 then
            node
        else
            let converted = List.map (fun x -> splitFunc x) funcList

            let rec reduce (arr: (string * float) list) (item: int) =
                if item = arr.Length then
                    arr
                else
                    let same = List.filter (fun x -> fst arr[item] = fst x) arr
                    let notsame = List.filter (fun x -> fst arr[item] <> fst x) arr
                    let sum = (fst same[0], List.sumBy (fun x -> snd x) same)

                    if snd sum <> 0 then
                        reduce (List.append [ sum ] notsame) (item + 1)
                    else
                        reduce notsame (item)

            let result = reduce converted 0

            if result.Length = 0 then
                { Value = Some(1.) // why 1?
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

let rec multiplyFuncs (node: Node) =
    match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
    | (true, true, val1) when val1 = "*" || val1 = "^" -> 
        if 
            // simplify this shit
            ((node.Left.Value.Operation = "x" && (isConst node.Right.Value || isComplexFunction node.Right.Value)) 
            || (node.Right.Value.Operation = "x" && (isConst node.Left.Value || isComplexFunction node.Left.Value)))
            || ((isConst node.Left.Value && isComplexFunction node.Right.Value) 
            || (isConst node.Right.Value && isComplexFunction node.Left.Value))
        then
            node
        else
            simplifyMultiply node
    | (true, true, val1) when val1 = "+" || val1 = "-" ->
        if
            not (isConst node.Left.Value)
            && node.Left.Value.Operation <> "+"
            && node.Left.Value.Operation <> "-"
            && not (isConst node.Left.Value)
            && node.Right.Value.Operation <> "+"
            && node.Right.Value.Operation <> "-"
        then
            { Value = None
              Operation = node.Operation
              Left = Some(simplifyMultiply node.Left.Value)
              Right = Some(simplifyMultiply node.Right.Value) }
        elif
            not (isConst node.Left.Value)
            && node.Left.Value.Operation <> "+"
            && node.Left.Value.Operation <> "-"
        then
            { Value = None
              Operation = node.Operation
              Left = Some(simplifyMultiply node.Left.Value)
              Right = Some(multiplyFuncs node.Right.Value) }
        elif
            not (isConst node.Right.Value)
            && node.Right.Value.Operation <> "+"
            && node.Right.Value.Operation <> "-"
        then
            { Value = None
              Operation = node.Operation
              Left = Some(multiplyFuncs node.Left.Value)
              Right = Some(simplifyMultiply node.Right.Value) }
        else
            { Value = None
              Operation = node.Operation
              Left = Some(multiplyFuncs node.Left.Value)
              Right = Some(multiplyFuncs node.Right.Value) }

    | _ -> node

let isSimple (node: Node) =
    if 
        node.Operation = "x"
        || isConst node
        || isComplexFunction node
        || (node.Operation = "^" || node.Operation = "*") 
               && ((node.Left.Value.Operation = "x" && isConst node.Right.Value) 
               || (node.Right.Value.Operation = "x" && isConst node.Left.Value))
    then
        true 
    else
        false

// add cache for sin(x), x ...
let simplifySum (node: Node) =
    let (|Sum|Another|) (node: Node) =
        if node.Left.IsSome && node.Right.IsSome && (node.Operation = "+" || node.Operation = "-") then
            Sum
        else
            Another

    let (|Both|Left|LeftNeg|Right|None|) (node: Node) =
        if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
            Both
        elif node.Left.Value.Value.IsSome && node.Operation = "-" then
            LeftNeg
        elif node.Left.Value.Value.IsSome then
            Left
        elif node.Right.Value.Value.IsSome then
            Right
        else
            None

    let mutable x0 = 0.

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
        match node with
        | Sum ->
            match node with
            | Both -> 
                let sum =
                    node.Left.Value.Value.Value
                    + changeIfNeg node.Operation node.Right.Value.Value.Value

                x0 <- x0 + sum

                { Value = Some(0.)
                  Operation = ""
                  Left = None
                  Right = None }
            | LeftNeg ->
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
            | Left ->
                x0 <- x0 + node.Left.Value.Value.Value

                sumNumbers node.Right.Value
            | Right ->
                x0 <- x0 + changeIfNeg node.Operation node.Right.Value.Value.Value

                sumNumbers node.Left.Value
            | None ->
                { Value = None
                  Operation = node.Operation
                  Left = Some(sumNumbers node.Left.Value)
                  Right = Some(sumNumbers node.Right.Value) }
        | Another -> node

    let rec clearNumbers (node: Node) : Node =
        match node with
        | Sum ->
            match node with
            | Both ->
                { Value = Some(0.)
                  Operation = ""
                  Left = None
                  Right = None }
            | LeftNeg when node.Left.Value.Value.Value <> 0 ->
                clearNumbers node.Right.Value
            | Right ->
                clearNumbers node.Left.Value
            | _ ->
                { Value = None
                  Operation = node.Operation
                  Left = Some(clearNumbers node.Left.Value)
                  Right = Some(clearNumbers node.Right.Value) }
        | Another -> node

    let rec recClearNumbers (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            recClearNumbers result (clearNumbers result)

    let rec sumFunctions (node: Node) =
        match node with
        | Sum ->
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
                 List.append [ changeFuncIfNeg node.Operation node.Right.Value ] [ node.Left.Value ]
            elif
                node.Left.Value.Operation = "*"
                || node.Left.Value.Operation = "^"
                || node.Left.Value.Operation = "x"
                || isComplexFunction node.Left.Value
            then
                List.append [ node.Left.Value ] (sumFunctions node.Right.Value)
            elif
                node.Right.Value.Operation = "*"
                || node.Right.Value.Operation = "^"
                || node.Right.Value.Operation = "x"
                || isComplexFunction node.Right.Value
            then
                List.append [ changeFuncIfNeg node.Operation node.Right.Value ] (sumFunctions node.Left.Value)
            else
                List.append (sumFunctions node.Left.Value) (sumFunctions node.Right.Value)
        | Another -> []

    let splitFuncs (node: Node) : float * string =
        let mutable coef = 1.
        let mutable sign = 1.

        let rec splitFuncs (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "*") ->
                if node.Left.Value.Operation <> "*" && node.Right.Value.Operation <> "*" then
                    [ node.Left.Value.ToString(); node.Right.Value.ToString() ]
                elif node.Left.Value.Operation <> "*" then
                    List.append [ node.Left.Value.ToString() ] (splitFuncs node.Right.Value)
                elif node.Right.Value.Operation <> "*" then
                    List.append [ node.Right.Value.ToString() ] (splitFuncs node.Left.Value)
                else
                    List.append (splitFuncs node.Left.Value) (splitFuncs node.Right.Value)
            | _ -> []

        let rec getWithoutCoef (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "-") ->
                sign <- -sign
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

        let result = getWithoutCoef node
        let funcs = splitFuncs result

        if funcs.Length = 0 then
            (coef * sign, result.ToString())
        else
            let sortedFunc = String.Join('*', List.sort funcs)
            (coef * sign, sortedFunc)

    let clearSumFunctions (node: Node) =
        let funcs = sumFunctions node

        if funcs.Length = 0 then
            node
        else
            let converted = List.map (fun x -> splitFuncs x) funcs

            let rec reduce (arr: (float * string) list) (item: int) =
                if item = arr.Length then
                    arr
                else
                    let same = List.filter (fun x -> snd arr[item] = snd x) arr
                    let notsame = List.filter (fun x -> snd arr[item] <> snd x) arr
                    let sum = (List.sumBy (fun x -> fst x) same, snd same[0])

                    if fst sum <> 0 then
                        reduce (List.append [ sum ] notsame) (item + 1)
                    else
                        reduce notsame (item)

            let result = reduce converted 0

            if result.Length = 0 then
                { Value = Some(0.)
                  Operation = ""
                  Left = None
                  Right = None }
            else
                let func =
                    String.Join('+', (List.map (fun x -> if fst x = 1. then $"{snd x}" else $"({fst x})*({snd x})") result))

                convertToFunc func
    
    match isSimple node with
    | true -> node
    | false ->
        let preproc = multiplyFuncs node |> sumNumbers

        let result= 
            clearNumbers preproc 
            |> recClearNumbers preproc 
            |> clearSumFunctions

        if x0 = 0 then
            result
        elif result.Value.IsSome && result.Value.Value = 0 then
            { Value = Some(x0)
                      Operation = ""
                      Left = None
                      Right = None }
        else
            { Value = None
              Operation = "+"
              Left = Some(result)
              Right =
                Some(
                    { Value = Some(x0)
                      Operation = ""
                      Left = None
                      Right = None }
                ) }

let rec multiplyAll (node: Node) =
    let (|Sum|Another|) (node: Node) =
        if node.Left.IsSome && node.Right.IsSome && (node.Operation = "+" || node.Operation = "-") then
            Sum
        else
            Another

    let (|Both|Left|Right|None|) (node: Node) =
        if (node.Left.Value.Operation <> "+" && node.Left.Value.Operation <> "-")
            && (node.Right.Value.Operation <> "+" && node.Right.Value.Operation <> "-") then
            Both
        elif node.Left.Value.Operation <> "+" && node.Left.Value.Operation <> "-" then
            Left
        elif node.Right.Value.Operation <> "+" && node.Right.Value.Operation <> "-" then
            Right
        else
            None
    // what about division
    let isNotComplexExpression (node: Node) =
        (node.Operation = "x" || isConst node || isComplexFunction node)
        || node.Operation = "*" || node.Operation = "^"
    
    let rec multiplyBy (isNeg: bool) (multiplier: Node) (right: Node) =
        match right with
        | Sum ->
            match right with
            | Both ->
                if isNeg then 
                    {  Value = None
                       Operation = 
                           match right.Operation with
                           | "+" -> "-"
                           | _ -> "+"
                       Left =
                           Some(
                               if right.Left.Value.Value.IsSome && right.Left.Value.Value.Value < 0 then
                                   { Value = None
                                     Operation = "*"
                                     Left = Some(multiplier)
                                     Right = right.Left }
                               else
                                   { Value = None
                                     Operation = "-"
                                     Left = Some(
                                        { Value = Some(0.)
                                          Operation = ""
                                          Left = None
                                          Right = None }
                                     )
                                     Right = Some(
                                        { Value = None
                                          Operation = "*"
                                          Left = Some(multiplier)
                                          Right = right.Left }
                                     ) }
                           )
                       Right =
                           Some(
                               { Value = None
                                 Operation = "*"
                                 Left = Some(multiplier)
                                 Right = right.Right }
                           ) }
                else
                    {   Value = None
                        Operation = right.Operation
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
            | Left ->
                { Value = None
                  Operation = "+"
                  Left =
                    Some(
                        if (isNeg && not (right.Operation = "-"))
                           || (not isNeg && right.Operation = "-") then
                            { Value = None
                              Operation = "-"
                              Left = Some(
                              {   Value = Some(0.)
                                  Operation = ""
                                  Left = None
                                  Right = None }
                              )
                              Right = Some(
                              {   Value = None
                                  Operation = "*"
                                  Left = Some(multiplier)
                                  Right = right.Left }
                              ) }
                        else
                            { Value = None
                              Operation = "*"
                              Left = Some(multiplier)
                              Right = right.Left }
                    )
                  Right = Some(multiplyBy isNeg multiplier right.Right.Value) }
            | Right ->
                { Value = None
                  Operation = "+"
                  Left = Some(multiplyBy isNeg multiplier right.Left.Value)
                  Right =
                    Some(
                        if (isNeg && not (right.Operation = "-"))
                           || (not isNeg && right.Operation = "-") then
                           { Value = None
                             Operation = "-"
                             Left = Some(
                             {   Value = Some(0.)
                                 Operation = ""
                                 Left = None
                                 Right = None }
                             )
                             Right = Some(
                             {   Value = None
                                 Operation = "*"
                                 Left = Some(multiplier)
                                 Right = right.Right }
                             ) }
                        else
                            { Value = None
                              Operation = "*"
                              Left = Some(multiplier)
                              Right = right.Right }
                    ) }
            | None ->
                { Value = None
                  Operation = right.Operation
                  Left = Some(multiplyBy isNeg multiplier right.Left.Value)
                  Right = Some(multiplyBy isNeg multiplier right.Right.Value) }
        | Another ->
            multiplyAll 
                { Value = None
                  Operation = "*"
                  Left = Some(multiplier)
                  Right = Some(right) }

    let rec operation (left: Node) (right: Node) =
        match left with
        | Sum ->
            match left with
            | Both ->
                if left.Left.Value.Value.IsSome && left.Left.Value.Value.Value = 0. then
                    (multiplyBy (left.Operation = "-") left.Right.Value right)
                else
                    { Value = None
                      Operation = "+"
                      Left = Some(multiplyBy false left.Left.Value right)
                      Right = Some(multiplyBy (left.Operation = "-") left.Right.Value right) }
            | Left ->
                { Value = None
                  Operation = "+"
                  Left = Some(multiplyBy (left.Operation = "-") left.Left.Value right)
                  Right = Some(operation left.Right.Value right) }
            | Right ->
                { Value = None
                  Operation = "+"
                  Left = Some(operation left.Left.Value right)
                  Right = Some(multiplyBy (left.Operation = "-") left.Right.Value right) }
            | None ->
                { Value = None
                  Operation = "+"
                  Left = Some(operation left.Left.Value right)
                  Right = Some(operation left.Right.Value right) }
        | Another -> 
            multiplyAll 
                { Value = None
                  Operation = "*"
                  Left = Some(left)
                  Right = Some(right) }

    let rec findBranch (node: Node) =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, "*") ->
            if
                (node.Left.Value.Operation = "+" || node.Left.Value.Operation = "-")
                && (node.Right.Value.Operation = "+" || node.Right.Value.Operation = "-")
            then
                operation (multiplyAll node.Left.Value) (multiplyAll node.Right.Value) 
            elif
                (node.Left.Value.Operation = "+" || node.Left.Value.Operation = "-")
                && (isNotComplexExpression node.Right.Value)
            then
                multiplyBy (node.Right.Value.Operation = "-") (multiplyAll node.Right.Value) (multiplyAll node.Left.Value) 
            elif
                (node.Right.Value.Operation = "+" || node.Right.Value.Operation = "-")
                && (isNotComplexExpression node.Left.Value)
            then
                multiplyBy (node.Left.Value.Operation = "-") (multiplyAll node.Left.Value) (multiplyAll node.Right.Value) 
            else
                { Value = None
                  Operation = "*"
                  Left = Some(findBranch node.Left.Value)
                  Right = Some(findBranch node.Right.Value) }
        | (true, true, val1) ->
            { Value = None
              Operation = val1
              Left = Some(findBranch node.Left.Value)
              Right = Some(findBranch node.Right.Value) }
        | _ -> node

    let rec recClear (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            recClear result (findBranch result)

    recClear node (findBranch node) |> simplifySum
