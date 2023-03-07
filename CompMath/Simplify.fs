module Simplify

open Node
open System

let isSumOrDif (node: Node) =
    node.Operation = '+' || node.Operation = '-'

let simplifyMultiply (node: Node) =
    // simplify
    let (|Multiply|Minus|Another|) (node: Node) =
        if node.Left.IsSome && node.Right.IsSome && node.Operation = '*' then
            Multiply
        elif node.Left.IsSome && node.Right.IsSome && node.Operation = '-' then
            Minus
        else
            Another

    let (|Both|Left|Right|None|) (node: Node) =
        if isConst node.Left.Value && isConst node.Right.Value then
            Both
        elif isConst node.Left.Value then
            Left
        elif isConst node.Right.Value then
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
                    x0 <- x0 * node.Left.Value.Value * node.Right.Value.Value
                    getNumber 0
                | Left ->
                    x0 <- x0 * node.Left.Value.Value
                    multiplyNumbers node.Right.Value
                | Right ->
                    x0 <- x0 * node.Right.Value.Value
                    multiplyNumbers node.Left.Value
                | None ->
                    combine
                        node.Operation
                        (multiplyNumbers node.Left.Value)
                        (multiplyNumbers node.Right.Value)
        | Minus ->
            if isConst node.Right.Value then
                x0 <- x0 * -node.Right.Value.Value
                getNumber -node.Right.Value.Value
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
            | Both -> getNumber 0
            | Left -> clearNumbers node.Right.Value
            | Right -> clearNumbers node.Left.Value
            | None ->
                combine
                    node.Operation
                    (clearNumbers node.Left.Value)
                    (clearNumbers node.Right.Value)
        | Minus
        | Another -> node

    let splitFunc (node: Node) : (string * float) =
        let mutable temp = 0.
        let mutable coef = 1.
        let mutable sign = 1.

        let rec getWithoutDegree (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, '^') ->
                if isConst node.Right.Value then
                    coef <- node.Right.Value.Value
                    node.Left.Value
                else
                    combine
                        node.Operation
                        (getWithoutDegree node.Left.Value)
                        (getWithoutDegree node.Right.Value)
            | _ -> node

        let rec splitFuncs (node: Node) : string list =
            match node with
            | Minus -> 
                 sign <- -sign
                 splitFuncs node.Right.Value
            | Multiply ->
                match (node.Left.Value.Operation, node.Right.Value.Operation) with
                | (val1, val2) when val1 <> '*' && val2 <> '*' ->
                    [ node.Left.Value.ToString(); node.Right.Value.ToString() ]
                | (_, '*') -> List.append [ node.Left.Value.ToString() ] (splitFuncs node.Right.Value)
                | ('*', _) -> List.append [ node.Right.Value.ToString() ] (splitFuncs node.Left.Value)
                | _ -> List.append (splitFuncs node.Left.Value) (splitFuncs node.Right.Value)
            | Another -> []

        let clearedNode = getWithoutDegree node
        let funcs = splitFuncs clearedNode

        if funcs.Length = 0 then
            (clearedNode.ToString(), coef)
        else
            let degree = List.filter (fun x -> isNumber x &temp) funcs

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
                String.Join('*', List.filter (fun x -> not (isNumber x &temp)) funcs |> List.sort)

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
                getNumber 1 // why 1?
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
        getNumber 0
    | _ ->
        let result = clearNumbers data |> clearFuncs

        if x0 <> 1 && not (isConst result) then
            combine
                '*'
                (getNumber x0)
                result
        else
            result

let isSimple (node: Node) =
    if 
        node.Operation = 'x'
        || isConst node
        || isComplexFunction node
        || (node.Operation = '^' || node.Operation = '*') 
               && ((node.Left.Value.Operation = 'x' && isConst node.Right.Value) 
               || (node.Right.Value.Operation = 'x' && isConst node.Left.Value))
    then
        true 
    else
        false

let simplifySum (node: Node) =
    let (|Sum|Another|) (node: Node) =
        if node.Left.IsSome && node.Right.IsSome && (node.Operation = '+' || node.Operation = '-') then
            Sum
        else
            Another

    let (|Both|Left|LeftNeg|Right|None|) (node: Node) =
        if isConst node.Left.Value && isConst node.Right.Value then
            Both
        elif isConst node.Left.Value && node.Operation = '-' then
            LeftNeg
        elif isConst node.Left.Value then
            Left
        elif isConst node.Right.Value then
            Right
        else
            None

    let mutable x0 = 0.

    let changeIfNeg (op: char) (number: float) = if op = '-' then -number else number

    let changeFuncIfNeg (op: char) (node: Node) =
        if op = '-' then
            combine
                '-'
                (getNumber 0)
                node
        else
            node

    let rec sumNumbers (node: Node) : Node =
        match node with
        | Sum ->
            match node with
            | Both -> 
                let sum =
                    node.Left.Value.Value
                    + changeIfNeg node.Operation node.Right.Value.Value

                x0 <- x0 + sum

                getNumber 0
            | LeftNeg ->
                x0 <- x0 + node.Left.Value.Value

                combine
                    node.Operation
                    (getNumber 0)
                    (sumNumbers node.Right.Value)
            | Left ->
                x0 <- x0 + node.Left.Value.Value

                sumNumbers node.Right.Value
            | Right ->
                x0 <- x0 + changeIfNeg node.Operation node.Right.Value.Value

                sumNumbers node.Left.Value
            | None ->
                combine
                    node.Operation
                    (sumNumbers node.Left.Value)
                    (sumNumbers node.Right.Value)
        | Another -> node

    let rec clearNumbers (node: Node) : Node =
        match node with
        | Sum ->
            match node with
            | Both -> getNumber 0               
            | LeftNeg when node.Left.Value.Value <> 0 ->
                clearNumbers node.Right.Value
            | Right ->
                clearNumbers node.Left.Value
            | _ ->
                combine
                    node.Operation
                    (clearNumbers node.Left.Value)
                    (clearNumbers node.Right.Value)
        | Another -> node

    let rec recClearNumbers (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            recClearNumbers result (clearNumbers result)

    // what???
    let rec sumFunctions (node: Node) =
        match node with
        | Sum ->
            if
                (node.Left.Value.Operation = '*'
                 || node.Left.Value.Operation = '^'
                 || node.Left.Value.Operation = 'x'
                 || isComplexFunction node.Left.Value)
                && (node.Right.Value.Operation = '*'
                    || node.Right.Value.Operation = '^'
                    || node.Right.Value.Operation = 'x'
                    || isComplexFunction node.Right.Value)
            then
                 List.append [ changeFuncIfNeg node.Operation node.Right.Value ] [ node.Left.Value ]
            elif
                node.Left.Value.Operation = '*'
                || node.Left.Value.Operation = '^'
                || node.Left.Value.Operation = 'x'
                || isComplexFunction node.Left.Value
            then
                List.append [ node.Left.Value ] (sumFunctions node.Right.Value)
            elif
                node.Right.Value.Operation = '*'
                || node.Right.Value.Operation = '^'
                || node.Right.Value.Operation = 'x'
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
            | (true, true, '*') ->
                if node.Left.Value.Operation <> '*' && node.Right.Value.Operation <> '*' then
                    [ node.Left.Value.ToString(); node.Right.Value.ToString() ]
                elif node.Left.Value.Operation <> '*' then
                    List.append [ node.Left.Value.ToString() ] (splitFuncs node.Right.Value)
                elif node.Right.Value.Operation <> '*' then
                    List.append [ node.Right.Value.ToString() ] (splitFuncs node.Left.Value)
                else
                    List.append (splitFuncs node.Left.Value) (splitFuncs node.Right.Value)
            | _ -> []

        let rec getWithoutCoef (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, '-') ->
                sign <- -sign
                getWithoutCoef node.Right.Value
            | (true, true, '*') ->
                if isConst node.Left.Value then
                    coef <- node.Left.Value.Value
                    node.Right.Value
                elif isConst node.Right.Value then
                    coef <- node.Right.Value.Value
                    node.Left.Value
                else
                    combine
                        node.Operation
                        (getWithoutCoef node.Left.Value)
                        (getWithoutCoef node.Right.Value)
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
                getNumber 0
            else
                let func =
                    String.Join('+', (List.map (fun x -> if fst x = 1. then $"{snd x}" else $"({fst x})*({snd x})") result))

                convertToFunc func
    
    match isSimple node with
    | true -> node
    | false ->
        let preproc = node |> sumNumbers

        let result= 
            clearNumbers preproc 
            |> recClearNumbers preproc 
            |> clearSumFunctions

        if x0 = 0 then
            result
        elif isConst result && result.Value = 0 then
            getNumber 0
        else
            combine
                '+'
                result
                (getNumber x0)

let rec findMulOrDiv (node: Node) =
    if node.Operation = '*' then
        printfn "%s * %s" (node.Left.Value.ToString())  (node.Right.Value.ToString())
        combine 
            node.Operation
            (findMulOrDiv node.Left.Value)
            (findMulOrDiv node.Right.Value)
    elif node.Operation = '/' then
        printfn "%s / %s" (node.Left.Value.ToString())  (node.Right.Value.ToString())
        combine 
            node.Operation
            (findMulOrDiv node.Left.Value)
            (findMulOrDiv node.Right.Value)
    elif isSumOrDif node then
        combine 
            node.Operation
            (findMulOrDiv node.Left.Value)
            (findMulOrDiv node.Right.Value)
    else
        node

let simplify (node: Node) =
    node
