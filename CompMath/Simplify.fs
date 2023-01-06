module Simplify

open Function
open System
open System.Text

let simplifyFunc (node: Node) =
    let mutable nt = 0.
    let mutable funcList: Node list = []

    let change (op: string) (number: float) =
        if op = "-" then
            -number
        else
            number

    let changeFunc (op: string) (node: Node) =
        if op = "-" then
            { Value = None; Operation = "-"; Left = Some({ Value = Some(0.); Operation = ""; Left = None; Right = None; }); Right = Some(node); }
        else
            node

    let rec sumNumbers (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" -> 
            if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
                let sum = node.Left.Value.Value.Value + change node.Operation node.Right.Value.Value.Value
                nt <- nt + sum
                { Value = Some(0.); Operation = ""; Left = None; Right = None}
            // -x = 0 - x
            elif node.Left.Value.Value.IsSome && node.Operation = "-" then
                nt <- nt + node.Left.Value.Value.Value
                { Value = None; Operation = node.Operation; Left = Some({ Value = Some(0.); Operation = ""; Left = None; Right = None; }); 
                    Right = Some(sumNumbers node.Right.Value) }
            elif node.Left.Value.Value.IsSome then
                nt <- nt + node.Left.Value.Value.Value
                sumNumbers { Value = None; Operation = node.Right.Value.Operation; Left = node.Right.Value.Left; Right = node.Right.Value.Right }
            elif node.Right.Value.Value.IsSome then
                nt <- nt + change node.Operation node.Right.Value.Value.Value
                sumNumbers { Value = None; Operation = node.Left.Value.Operation; Left = node.Left.Value.Left; Right = node.Left.Value.Right }
            else
                { Value = None; Operation = node.Operation; Left = Some(sumNumbers node.Left.Value); Right = Some(sumNumbers node.Right.Value) }
        | _ -> node
      
    let rec clear (node: Node) : Node =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" -> 
            if node.Left.Value.Value.IsSome && node.Right.Value.Value.IsSome then
                { Value = Some(0.); Operation = ""; Left = None; Right = None}
            elif node.Left.Value.Value.IsSome && node.Left.Value.Value.Value <> 0 && node.Operation <> "-" then
                clear { Value = None; Operation = node.Right.Value.Operation; Left = node.Right.Value.Left; Right = node.Right.Value.Right }
            elif node.Right.Value.Value.IsSome then
                clear { Value = None; Operation = node.Left.Value.Operation; Left = node.Left.Value.Left; Right = node.Left.Value.Right }
            else
                { Value = None; Operation = node.Operation; Left = Some(clear node.Left.Value); Right = Some(clear node.Right.Value) }
        | _ -> node

    let rec clearFunc (last: Node) (result: Node) =
        if last.ToString() = result.ToString() then
            result
        else
            clearFunc result (clear result)

    let rec sumFunc (node: Node) =
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" -> 
            if (node.Left.Value.Operation = "*" || node.Left.Value.Operation = "^" || node.Left.Value.Operation = "x" || isComplexFunction node.Left.Value) && 
                (node.Right.Value.Operation = "*" || node.Right.Value.Operation = "^" || node.Right.Value.Operation = "x" || isComplexFunction node.Right.Value) then
                funcList <- List.append funcList [changeFunc val1 node.Right.Value] |> List.append [node.Left.Value]
            elif node.Left.Value.Operation = "*" || node.Left.Value.Operation = "^" || node.Left.Value.Operation = "x" || isComplexFunction node.Left.Value then
                funcList <- List.append funcList [node.Left.Value]
                sumFunc node.Right.Value
            elif node.Right.Value.Operation = "*" || node.Right.Value.Operation = "^" || node.Right.Value.Operation = "x" || isComplexFunction node.Right.Value then
                funcList <- List.append funcList [changeFunc val1 node.Right.Value]
                sumFunc node.Left.Value
            else
                sumFunc node.Left.Value
                sumFunc node.Right.Value
        | _ -> ()


    let split (node: Node) : float * string =
        let mutable coef = 1.
        let mutable isNeg = false

        let rec getWithoutCoef (node: Node) =
            match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
            | (true, true, "-") -> 
                isNeg <- not isNeg
                getWithoutCoef node.Right.Value
            | (true, true, "*") -> 
                if node.Left.Value.Value.IsSome then
                    coef <- node.Left.Value.Value.Value
                    node.Right.Value
                elif node.Right.Value.Value.IsSome then
                    coef <- node.Right.Value.Value.Value
                    node.Left.Value
                else
                    { Value = None; Operation = node.Operation; Left = Some(getWithoutCoef node.Left.Value); Right = Some(getWithoutCoef node.Right.Value) }
            | _ -> node

        let nod = getWithoutCoef node

        if isNeg then
            (-coef, nod.ToString())
        else
            (coef, nod.ToString())
            
    let clearSumFunc (node: Node) =
        sumFunc node

        if funcList.Length = 0 then
            node
        else
            let arr = List.map (fun x -> split x) funcList
            printfn "converted %A" arr

            let rec go (arr: (float * string) list) (item: int)=
                if item = arr.Length then 
                    arr
                else
                    let same = List.filter (fun x -> snd arr[item] = snd x) arr
                    let notsame = List.filter (fun x -> snd arr[item] <> snd x) arr
                    let sum = (List.sumBy (fun x -> fst x) same, snd same[0])

                    if fst sum <> 0 then
                        go (List.append [sum] notsame) (item + 1)
                    else
                        go notsame (item)

            let data = go arr 0
            printfn "arr: %A" data

            let func = String.Join('+', (List.map (fun x -> if fst x = 1. then $"{snd x}" else $"({fst x})*({snd x})" ) data))
            convertToFunc func

    let pre = sumNumbers node
    let result = clearFunc pre (clear pre)
    let data = clearSumFunc result

    printfn "data: %s" (data.ToString())
    printfn "nt: %f" nt

    if nt <> 0 then
        { Value = None; Operation = "+"; Left = Some(data); Right = Some({ Value = Some(nt); Operation = ""; Left = None; Right = None; }) }
    else
        data

let openBrackets (node: Node) =
    let mutable clear = true

    let op (node: Node) =
        let func = node.ToString()
        let ind = func.IndexOf("-(")

        if ind = -1 then
            node
        else
            clear <- false

            let inner = func[ind + 1..]

            let mutable (l, r) = getBracketsIndexesRight2Left inner
            let rfirst = r

            let temp = inner[..r].ToCharArray()

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
                    brackets <- List.append brackets [(l, r)]

            let builder = new StringBuilder(func[..ind])

            let mutable item = 0
            let mutable i = 1

            while i < rfirst do
                if brackets.Length <> 0 && fst brackets[item] = i then
                    builder.Append inner[fst brackets[item]..snd brackets[item]] |> ignore
                    i <- snd brackets[item]
                    if item + 1 <> brackets.Length then item <- item + 1
                    else ()
                elif temp[i] = '+' then
                    builder.Append '-' |> ignore
                elif temp[i] = '-' then
                    builder.Append '+' |> ignore
                else
                    builder.Append temp[i] |> ignore

                i <- i + 1
    
            builder.Append(inner[rfirst + 1..]) |> ignore

            convertToFunc (builder.ToString()) 

    let rec clearFunc (result: Node) =
        if clear then result
        else 
            clear <- true
            clearFunc (op result)

    clearFunc (op node)

let multiplyFunc (node: Node) =
    let rec multiplyBy (multiplier: Node) (right: Node) = 
        match (right.Left.IsSome, right.Right.IsSome, right.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" -> 
            if (right.Left.Value.Operation <> "+" && right.Left.Value.Operation <> "-") &&
                (right.Right.Value.Operation <> "+" && right.Right.Value.Operation <> "-") then
                { Value = None; Operation = val1; Left = Some({Value = None; Operation = "*"; Left = Some(multiplier); Right = right.Left });
                    Right = Some({Value = None; Operation = "*"; Left = Some(multiplier); Right = right.Right }) }
            elif (right.Left.Value.Operation <> "+" && right.Left.Value.Operation <> "-") then
                { Value = None; Operation = val1; Left = Some({Value = None; Operation = "*"; Left = Some(multiplier); Right = right.Left });
                    Right = Some(multiplyBy multiplier right.Right.Value) }
            elif (right.Right.Value.Operation <> "+" && right.Right.Value.Operation <> "-") then
                { Value = None; Operation = val1; Left = Some(multiplyBy multiplier right.Left.Value);
                    Right = Some({Value = None; Operation = "*"; Left = Some(multiplier); Right = right.Right }) }
            else 
                { Value = None; Operation = val1; Left = Some(multiplyBy multiplier right.Left.Value);
                    Right = Some(multiplyBy multiplier right.Right.Value) }
        | _ -> failwith "what???"

    let rec operation (left: Node) (right: Node) =
        match (left.Left.IsSome, left.Right.IsSome, left.Operation) with
        | (true, true, val1) when val1 = "+" || val1 = "-" -> 
            if (left.Left.Value.Operation <> "+" && left.Left.Value.Operation <> "-") &&
                (left.Right.Value.Operation <> "+" && left.Right.Value.Operation <> "-") then
                { Value = None; Operation = val1; Left = Some(multiplyBy left.Left.Value right);
                    Right = Some(multiplyBy left.Right.Value right); }
            elif (left.Left.Value.Operation <> "+" && left.Left.Value.Operation <> "-") then
                { Value = None; Operation = val1; Left = Some(multiplyBy left.Left.Value right);
                    Right = Some(operation left.Right.Value right); }
            elif (left.Right.Value.Operation <> "+" && left.Right.Value.Operation <> "-") then
                { Value = None; Operation = val1; Left = Some(operation left.Left.Value right);
                    Right = Some(multiplyBy left.Right.Value right); }
            else 
                { Value = None; Operation = val1; Left = Some(operation left.Left.Value right);
                    Right = Some(operation left.Right.Value right); }
        | _ -> failwith "what???"

    let rec findMul (node: Node) = 
        match (node.Left.IsSome, node.Right.IsSome, node.Operation) with
        | (true, true, "*") -> 
            if (node.Left.Value.Operation = "+"  || node.Left.Value.Operation = "-") &&
                (node.Right.Value.Operation = "+" || node.Right.Value.Operation = "-") then
                    operation node.Left.Value node.Right.Value
            elif (node.Left.Value.Operation = "+"  || node.Left.Value.Operation = "-") then
                    multiplyBy node.Right.Value node.Left.Value
            elif (node.Right.Value.Operation = "+" || node.Right.Value.Operation = "-") then
                    multiplyBy node.Left.Value node.Right.Value
            else 
                { Value = None; Operation = "*"; Left = Some(findMul node.Left.Value);
                    Right = Some(findMul node.Right.Value); }
        | (true, true, val1) -> { Value = None; Operation = val1; Left = Some(findMul node.Left.Value);
                                Right = Some(findMul node.Right.Value); }
        | _ -> node

    openBrackets (findMul node)  