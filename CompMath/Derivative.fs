﻿module Derivative

open Node

let derivativeFunc (node: Node) (d: char) : Node =   
    let rec op (node: Node) : Node =
        match node.Operation with
        | val1 when isConst(node) ->
            getNumber 0
        | val1 when val1 = d ->
            getNumber 1 
        | '+'
        | '-' ->
            { Value = 0
              Operation = node.Operation
              Left = Some(op node.Left.Value)
              Right = Some(op node.Right.Value) }
        | '*' -> 
            let leftConst = isConst node.Left.Value
            let rightConst = isConst node.Right.Value

            // x * a
            if node.Left.Value.Operation = d && rightConst then
                node.Right.Value
            // a * x
            elif node.Right.Value.Operation = d && leftConst then
                node.Left.Value
            // a * f(x)
            elif leftConst then
                combine 
                    (node.Left.Value) 
                    (op node.Right.Value) 
                    '*'
            // f(x) * a
            elif rightConst then
                combine 
                    (op node.Left.Value) 
                    (node.Right.Value) 
                    '*'
            // f(x) * f(x)
            else
                combine 
                    (combine (op node.Left.Value) node.Right.Value '*')
                    (combine node.Left.Value (op node.Right.Value) '*')
                    '+'
        | '/' -> 
            let leftConst = isConst node.Left.Value
            let rightConst = isConst node.Right.Value

            // a/x
            if leftConst && node.Right.Value.Operation = d then
                combine 
                    (getNumber 0)
                    (combine
                        node.Left.Value
                        (combine
                            (getParameter d)
                            (getNumber 2)
                            '^')
                        '/')
                    '-'
            // x/a
            elif rightConst && node.Left.Value.Operation = d then
                combine 
                    (getNumber 1)
                    node.Right.Value
                    '/'
            // a/f(x)
            elif leftConst then
                combine 
                    (getNumber 0)
                    (combine
                        (combine
                            (node.Left.Value)
                            (op node.Right.Value)
                            '*')
                        (combine
                            node.Right.Value
                            (getNumber 2)
                            '^')
                        '/')
                    '-'
            // f(x)/a
            elif rightConst then
                combine
                    (op node.Left.Value)
                    node.Right.Value
                    '/'
            // f(x)/f(x)
            else
                combine 
                    (combine
                        (combine
                            (op node.Left.Value)
                            node.Right.Value
                            '*')
                        (combine
                            node.Left.Value
                            (op node.Right.Value)
                            '*')
                        '-')
                    (combine
                        node.Right.Value
                        (getNumber 2)
                        '^')
                    '/'
        | '^' -> 
            let leftConst = isConst node.Left.Value
            let rightConst = isConst node.Right.Value

            // x^a
            if node.Left.Value.Operation = d && rightConst then
                combine
                    node.Right.Value
                    (combine
                        node.Left.Value
                        (getNumber (node.Right.Value.Value - 1.))
                        '^')
                    '*'
            // a^x
            elif leftConst && node.Right.Value.Operation = d then
                combine
                    node
                    (getFunction node.Left.Value "ln")
                    '*'
            // f(x)^a
            elif rightConst then
                combine
                    (combine
                        node.Right.Value
                        (combine
                            node.Left.Value
                            (getNumber (node.Right.Value.Value - 1.))
                            '^')
                        '*')
                    (op node.Left.Value)
                    '*'
            // a^f(x)
            elif leftConst then
                combine
                    (combine
                        node
                        (getFunction node.Left.Value "ln")
                        '*')
                    (op node.Right.Value)
                    '*'
            // f(x)^f(x)
            else
                op
                    (getFunction 
                        (combine 
                            node.Right.Value
                            (getFunction node.Left.Value "ln")
                            '*')
                        "exp") 
        //| '|' -> 
        //    // |x|
        //    if node.Right.Value.Operation = d then
        //        getFunction node.Right.Value "sgn"
        //    // |f(x)|
        //    else
        //        combin
        | '\u0010' -> 
            // ln(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 1)
                    node.Right.Value
                    '/'
            // ln(f(x))
            else
                combine
                    (op node.Right.Value)
                    node.Right.Value
                    '/'
        | '\u0011' -> 
            // lg(f(x))
            combine
                (op (getFunction node.Right.Value "ln"))
                (getFunction (getNumber 10) "ln")
                '/'
        | '\u0012' -> 
            // sin(x)
            if node.Right.Value.Operation = d then
                getFunction node.Right.Value "cos"
            // sin(f(x))
            else
                combine 
                    (getFunction node.Right.Value "cos")
                    (op node.Right.Value)
                    '*'
        | '\u0013' -> 
            // cos(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 0)
                    (getFunction node.Right.Value "sin")
                    '-'
            // cos(f(x))
            else
                combine 
                    (combine
                        (getNumber 0)
                        (getFunction node.Right.Value "sin")
                        '-')
                    (op node.Right.Value)
                    '*'
        | '\u0014' -> 
            // sqrt(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 1)
                    (combine
                        (getNumber 2)
                        node
                        '*')
                    '/'
            // sqrt(f(x))
            else
                combine 
                    (op node.Right.Value)
                    (combine
                        (getNumber 2)
                        node
                        '*')
                    '/'
        | '\u0015' ->
            // log2(f(x))
            combine
                (op (getFunction node.Right.Value "ln"))
                (getFunction (getNumber 2) "ln")
                '/'
        | '\u0016' ->
            // tg(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 1)
                    (combine 
                        (getFunction node.Right.Value "cos")
                        (getNumber 2)
                        '^')
                    '/'
            // tg(f(x))
            else
                combine 
                    (op node.Right.Value)
                    (combine 
                        (getFunction node.Right.Value "cos")
                        (getNumber 2)
                        '^')
                    '/'
        | '\u0017' ->
            // ctg(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 0)
                    (combine 
                        (getNumber 1)
                        (combine 
                            (getFunction node.Right.Value "sin")
                            (getNumber 2)
                            '^')
                        '/')
                    '-'
            // ctg(f(x))
            else
                combine 
                    (getNumber 0)
                    (combine 
                        (op node.Right.Value)
                        (combine 
                            (getFunction node.Right.Value "sin")
                            (getNumber 2)
                            '^')
                        '/')
                    '-'
        | '\u0018' -> 
            // e^x
            if node.Right.Value.Operation = d then
                node
            // e^f(x)
            else
                combine 
                    node
                    (op node.Right.Value)
                    '*'
        | '\u0019' ->
            // arcsin(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 1)
                    (getFunction 
                        (combine
                            (getNumber 1)
                            (combine 
                                node.Right.Value
                                (getNumber 2)
                                '^')
                            '-')
                        "sqrt")
                    '/'
            // arcsin(f(x))
            else
                combine 
                    (op node.Right.Value)
                    (getFunction 
                        (combine
                            (getNumber 1)
                            (combine 
                                node.Right.Value
                                (getNumber 2)
                                '^')
                            '-')
                        "sqrt")
                    '/'
        | '\u001A' ->
            // arccos(x)
            if node.Right.Value.Operation = d then
                combine
                    (getNumber 0)
                    (combine 
                        (getNumber 1)
                        (getFunction 
                            (combine
                                (getNumber 1)
                                (combine 
                                    node.Right.Value
                                    (getNumber 2)
                                    '^')
                                '-')
                            "sqrt")
                        '/')
                    '-'
            // arccos(f(x))
            else
                combine
                    (getNumber 0)
                    (combine 
                        (op node.Right.Value)
                        (getFunction 
                            (combine
                                (getNumber 1)
                                (combine 
                                    node.Right.Value
                                    (getNumber 2)
                                    '^')
                                '-')
                            "sqrt")
                        '/')
                    '-'
        | '\u001B' ->
            // arctg(x)
            if node.Right.Value.Operation = d then
                combine 
                    (getNumber 1)
                    (combine
                        (getNumber 1)
                        (combine 
                            node.Right.Value
                            (getNumber 2)
                            '^')
                        '+')
                    '/'
            // arctg(f(x))
            else
                combine 
                    (op node.Right.Value)
                    (combine
                        (getNumber 1)
                        (combine 
                            node.Right.Value
                            (getNumber 2)
                            '^')
                        '+')
                    '/'
        | '\u001C' ->
            // arcctg(x)
            if node.Right.Value.Operation = d then
                combine
                    (getNumber 0)
                    (combine 
                        (getNumber 1)
                        (combine
                            (getNumber 1)
                            (combine 
                                node.Right.Value
                                (getNumber 2)
                                '^')
                            '+')
                        '/')
                    '-'
            // arcctg(f(x))
            else
                combine
                    (getNumber 0)
                    (combine 
                        (op node.Right.Value)
                        (combine
                            (getNumber 1)
                            (combine 
                                node.Right.Value
                                (getNumber 2)
                                '^')
                            '+')
                        '/')
                    '-'
        | '\u001D' -> getNumber 0
        | _ -> raise (UnknownOperation (string node.Operation))

    op node
