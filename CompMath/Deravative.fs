module Derivative

open Node

let rec derivativeFunc (node: Node) (d: string) : Node =
    match node.Operation with
    | val1 when isConst(node) ->
        { Value = Some(0.)
          Operation = ""
          Left = None
          Right = None }
    | val1 when val1 = d ->
        { Value = Some(1.)
          Operation = ""
          Left = None
          Right = None } 
    | "+"
    | "-" ->
        { Value = None
          Operation = node.Operation
          Left = Some(derivativeFunc node.Left.Value d)
          Right = Some(derivativeFunc node.Right.Value d) }
    | "*" -> 
        // x * a
        if node.Left.Value.Operation = d && isConst node.Right.Value then
            node.Right.Value
        // a * x
        elif node.Right.Value.Operation = d && isConst node.Left.Value then
            node.Left.Value
        // a * f(x)
        elif isConst node.Left.Value then
            { Value = None
              Operation = "*"
              Left = Some(derivativeFunc node.Right.Value d)
              Right = node.Left }
        // f(x) * a
        elif isConst node.Right.Value then
            { Value = None
              Operation = "*"
              Left = Some(derivativeFunc node.Left.Value d)
              Right = node.Right }
        // f(x) * f(x)
        else
            { Value = None
              Operation = "+"
              Left =
                Some(
                    { Value = None
                      Operation = "*"
                      Left = Some(derivativeFunc node.Left.Value d)
                      Right = Some(node.Right.Value) }
                )
              Right =
                Some(
                    { Value = None
                      Operation = "*"
                      Left = Some(node.Left.Value)
                      Right = Some(derivativeFunc node.Right.Value d) }
                ) }
    | "/" -> // a/x
        if isConst node.Left.Value && node.Right.Value.Operation = d then
            { Value = None
              Operation = "-"
              Left = Some(convertToFunc "0")
              Right =
                Some(
                    { Value = None
                      Operation = "/"
                      Left = node.Left
                      Right =
                        Some(
                            { Value = None
                              Operation = "^"
                              Left =
                                Some(
                                    { Value = None
                                      Operation = d
                                      Left = None
                                      Right = None }
                                )
                              Right =
                                Some(
                                    { Value = Some(2.)
                                      Operation = ""
                                      Left = None
                                      Right = None }
                                ) }
                        ) }
                ) }
        // x/a
        elif isConst node.Right.Value && node.Left.Value.Operation = d then
            { Value = None
              Operation = "/"
              Left =
                Some(
                    { Value = Some(1.)
                      Operation = ""
                      Left = None
                      Right = None }
                )
              Right = node.Right }
        // a/f(x)
        elif isConst node.Left.Value then
            { Value = None
              Operation = "-"
              Left =
                Some(
                    { Value = Some(0.)
                      Operation = ""
                      Left = None
                      Right = None }
                )
              Right =
                Some(
                    { Value = None
                      Operation = "/"
                      Left = Some(derivativeFunc node.Right.Value d)
                      Right =
                        Some(
                            { Value = None
                              Operation = "^"
                              Left = node.Right
                              Right =
                                Some(
                                    { Value = Some(2.)
                                      Operation = ""
                                      Left = None
                                      Right = None }
                                ) }
                        ) }
                ) }
        // f(x)/a
        elif isConst node.Right.Value then
            { Value = None
              Operation = "*"
              Left =
                Some(
                    { Value = None
                      Operation = "/"
                      Left =
                        Some(
                            { Value = Some(1.)
                              Operation = ""
                              Left = None
                              Right = None }
                        )
                      Right = node.Right }
                )
              Right = Some(derivativeFunc node.Left.Value d) }
        // f(x)/f(x)
        else
            { Value = None
              Operation = "/"
              Left =
                Some(
                    { Value = None
                      Operation = "-"
                      Left =
                        Some(
                            { Value = None
                              Operation = "*"
                              Left = Some(derivativeFunc node.Left.Value d)
                              Right = node.Right }
                        )
                      Right =
                        Some(
                            { Value = None
                              Operation = "*"
                              Left = node.Left
                              Right = Some(derivativeFunc node.Right.Value d) }
                        ) }
                )
              Right =
                Some(
                    { Value = None
                      Operation = "^"
                      Left = node.Right
                      Right =
                        Some(
                            { Value = Some(2.)
                              Operation = ""
                              Left = None
                              Right = None }
                        ) }
                ) }
    | "^" -> // x^a
        if node.Left.Value.Operation = d && isConst node.Right.Value then
            { Value = None
              Operation = "*"
              Left = node.Right
              Right =
                Some(
                    { Value = None
                      Operation = "^"
                      Left = node.Left
                      Right =
                        Some(
                            { Value = Some(node.Right.Value.Value.Value - 1.)
                              Operation = ""
                              Left = None
                              Right = None }
                        ) }
                ) }
        // a^x
        elif isConst node.Left.Value then
            { Value = None
              Operation = "*"
              Left = Some(node)
              Right =
                Some(
                    { Value = None
                      Operation = "ln"
                      Left = None
                      Right = node.Left }
                ) }
        // f(x)^a
        elif isConst node.Right.Value then
            { Value = None
              Operation = "*"
              Left =
                Some(
                    { Value = None
                      Operation = "*"
                      Left = node.Right
                      Right =
                        Some(
                            { Value = None
                              Operation = "^"
                              Left = node.Left
                              Right =
                                Some(
                                    { Value = Some(node.Right.Value.Value.Value - 1.)
                                      Operation = ""
                                      Left = None
                                      Right = None }
                                ) }
                        ) }
                )
              Right = Some(derivativeFunc node.Left.Value d) }
        // a^f(x)
        elif isConst node.Left.Value then
            { Value = None
              Operation = "*"
              Left =
                Some(
                    { Value = None
                      Operation = "*"
                      Left = node.Right
                      Right =
                        Some(
                            { Value = None
                              Operation = "^"
                              Left = node.Left
                              Right =
                                Some(
                                    { Value = Some(node.Right.Value.Value.Value - 1.)
                                      Operation = ""
                                      Left = None
                                      Right = None }
                                ) }
                        ) }
                )
              Right = Some(derivativeFunc node.Left.Value d) }
        // f(x)^f(x)
        else
            derivativeFunc
                { Value = Some(0.)
                  Operation = "exp"
                  Left = None
                  Right =
                    Some(
                        { Value = None
                          Operation = "*"
                          Left = node.Right
                          Right =
                            Some(
                                { Value = None
                                  Operation = "ln"
                                  Left = None
                                  Right = node.Left }
                            ) }
                    ) } d
    | "exp" -> // e^x
        if node.Right.Value.Operation = d then
            node
        // e^f(x)
        else
            { Value = None
              Operation = "*"
              Left = Some(derivativeFunc node.Right.Value d)
              Right = Some(node) }
    | "ln" -> // ln(x)
        if node.Right.Value.Operation = d then
            { Value = None
              Operation = "/"
              Left =
                Some(
                    { Value = Some(1.)
                      Operation = ""
                      Left = None
                      Right = None }
                )
              Right = node.Right }
        // ln(f(x))
        else
            { Value = None
              Operation = "*"
              Left =
                Some(
                    { Value = None
                      Operation = "/"
                      Left =
                        Some(
                            { Value = Some(1.)
                              Operation = ""
                              Left = None
                              Right = None }
                        )
                      Right = node.Right }
                )
              Right = Some(derivativeFunc node.Right.Value d) }
    | "sin" -> // sin(x)
        if node.Right.Value.Operation = d then
            { Value = None
              Operation = "cos"
              Left = None
              Right = node.Right }
        // sin(f(x))
        else
            { Value = None
              Operation = "*"
              Left = Some(derivativeFunc node.Right.Value d)
              Right =
                Some(
                    { Value = None
                      Operation = "cos"
                      Left = None
                      Right = node.Right }
                ) }
    | "cos" -> // cos(x)
        if node.Right.Value.Operation = d then
            { Value = None
              Operation = "-"
              Left =
                Some(
                    { Value = Some(0.)
                      Operation = ""
                      Left = None
                      Right = None }
                )
              Right =
                Some(
                    { Value = None
                      Operation = "sin"
                      Left = None
                      Right = node.Right }
                ) }
        // cos(f(x))
        else
            { Value = None
              Operation = "*"
              Left = Some(derivativeFunc node.Right.Value d)
              Right =
                Some(
                    { Value = None
                      Operation = "-"
                      Left =
                        Some(
                            { Value = Some(0.)
                              Operation = ""
                              Left = None
                              Right = None }
                        )
                      Right =
                        Some(
                            { Value = None
                              Operation = "sin"
                              Left = None
                              Right = node.Right }
                        ) }
                ) }
    | _ -> failwith "Unknown operation"
