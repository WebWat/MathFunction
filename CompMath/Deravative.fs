module Deravative

open Function

let rec derivativeFunc (node: Node) : Node =
    match node.Operation with
    | "" | "pi" | "e" -> { Value = Some(0.); Operation = ""; Left = None; Right = None; }
    | "x" -> { Value = Some(1.); Operation = ""; Left = None; Right = None; }
    | "+" | "-" -> { Value = None; Operation = node.Operation; Left = Some(derivativeFunc node.Left.Value); Right = Some(derivativeFunc node.Right.Value); }
    | "*" -> // x * a
                if node.Left.Value.Operation = "x" && isConst node.Right.Value then
                    node.Right.Value
                // a * x
                elif node.Right.Value.Operation = "x" && isConst node.Left.Value then
                    node.Left.Value
                // a * f(x)
                elif isConst node.Left.Value then
                    { 
                        Value = None; Operation = "*"; 
                        Left = Some(derivativeFunc node.Right.Value); 
                        Right = node.Left;  
                    }
                // f(x) * a
                elif isConst node.Right.Value then
                    { 
                        Value = None; Operation = "*"; 
                        Left = Some(derivativeFunc node.Left.Value); 
                        Right = node.Right;  
                    }
                // f(x) * f(x)
                else
                    { 
                    Value = None; Operation = "+"; 
                    Left = Some(
                    { 
                        Value = None; Operation = "*"; 
                        Left = Some(derivativeFunc node.Left.Value); 
                        Right = Some(node.Right.Value); 
                    }); 
                    Right = Some(
                    { 
                        Value = None; Operation = "*"; 
                        Left = Some(node.Left.Value); 
                        Right = Some(derivativeFunc node.Right.Value); 
                    });  
                    }
    | "/" -> // a/x
                if isConst node.Left.Value && node.Right.Value.Operation = "x" then
                    { 
                        Value = None; Operation = "-"; 
                        Left = Some({ Value = Some(0.); Operation = ""; Left = None; Right = None; }) 
                        Right = Some(
                        { 
                            Value = None; Operation = "/"; 
                            Left = node.Left
                            Right = Some(
                            { 
                            Value = None; Operation = "^"; Left = Some({ Value = None; Operation = "x"; Left = None; Right = None; }); 
                            Right = Some({ Value = Some(2.); Operation = ""; Left = None; Right = None; }); 
                            });
                        });  
                    }
                // x/a
                elif isConst node.Right.Value && node.Left.Value.Operation = "x" then
                { 
                    Value = None; Operation = "/"; 
                    Left = Some({ Value = Some(1.); Operation = ""; Left = None; Right = None; });
                    Right = node.Right
                }
                // a/f(x)
                elif isConst node.Left.Value then
                    { 
                        Value = None; Operation = "-"; 
                        Left = Some({ Value = Some(0.); Operation = ""; Left = None; Right = None; }) 
                        Right = Some(
                        { 
                            Value = None; Operation = "/"; 
                            Left = Some(derivativeFunc node.Right.Value)
                            Right = Some(
                            { 
                            Value = None; Operation = "^"; Left = node.Right; 
                            Right = Some({ Value = Some(2.); Operation = ""; Left = None; Right = None; }); 
                            });
                        });  
                    }
                // f(x)/a
                elif isConst node.Right.Value then
                    {
                    Value = None; Operation = "*"; 
                    Left = Some({ 
                        Value = None; Operation = "/"; 
                        Left = Some({ Value = Some(1.); Operation = ""; Left = None; Right = None; });
                        Right = node.Right
                    });
                    Right = Some(derivativeFunc node.Left.Value)
                    }
                // f(x)/f(x)
                else
                    { 
                        Value = None; Operation = "/"; 
                        Left = Some(
                        { 
                            Value = None; Operation = "-"; 
                            Left = Some({ Value = Some(0.); Operation = "*"; Left = Some(derivativeFunc node.Left.Value); Right = node.Right; }) 
                            Right = Some({ Value = Some(0.); Operation = "*"; Left = node.Left; Right = Some(derivativeFunc node.Right.Value); }) 
                        }
                        );
                        Right = Some(
                        { 
                            Value = None; Operation = "^"; 
                            Left = node.Right
                            Right = Some({ Value = Some(2.); Operation = ""; Left = None; Right = None; });
                        });  
                    }
    | "^" -> // x^a
                if node.Left.Value.Operation = "x" && isConst node.Right.Value then
                { 
                    Value = None; Operation = "*"; 
                    Left = node.Right; 
                    Right = Some(
                    { 
                        Value = None; Operation = "^"; 
                        Left = node.Left; 
                        Right = Some({ Value = Some(node.Right.Value.Value.Value - 1.); Operation = ""; Left = None; Right = None; });
                    });  
                }
                // a^x
                elif isConst node.Left.Value then
                {
                    Value = None; Operation = "*"; 
                    Left = Some(node); 
                    Right = Some({ Value = None; Operation = "ln"; Left = None; Right = node.Left; });  
                }
                // f(x)^a
                elif isConst node.Right.Value then
                {
                    Value = None; Operation = "*"; 
                    Left = Some(
                    {                     
                        Value = None; Operation = "*"; 
                        Left = node.Right; 
                        Right = Some(
                        { 
                            Value = None; Operation = "^"; 
                            Left = node.Left; 
                            Right = Some({ Value = Some(node.Right.Value.Value.Value - 1.); Operation = ""; Left = None; Right = None; })
                        });  
                    } ); 
                    Right = Some(derivativeFunc node.Left.Value);  
                }
                // a^f(x)
                elif isConst node.Left.Value then
                {
                    Value = None; Operation = "*"; 
                    Left = Some(
                    {                     
                        Value = None; Operation = "*"; 
                        Left = node.Right; 
                        Right = Some(
                        { 
                            Value = None; Operation = "^"; 
                            Left = node.Left; 
                            Right = Some({ Value = Some(node.Right.Value.Value.Value - 1.); Operation = ""; Left = None; Right = None; })
                        });  
                    }); 
                    Right = Some(derivativeFunc node.Left.Value);  
                }
                // f(x)^f(x)
                else
                derivativeFunc { 
                    Value = Some(0.); Operation = "exp"; Left = None; 
                    Right = Some(
                    {                     
                        Value = None; Operation = "*"; 
                        Left = node.Right; 
                        Right = Some(
                        { 
                            Value = None; Operation = "ln"; Left = None; 
                            Right = node.Left
                        });  
                    });
                }
    | "exp" -> // e^x
                if node.Right.Value.Operation = "x" then
                    node
                // e^f(x)
                else 
                    { Value = None; Operation = "*"; Left = Some(derivativeFunc node.Right.Value); Right = Some(node); }
    | "ln" ->  // ln(x)
                if node.Right.Value.Operation = "x" then
                    { Value = None; Operation = "/"; Left = Some({ Value = Some(1.); Operation = ""; Left = None; Right = None; }); 
                        Right = node.Right; }
                // ln(f(x))
                else
                    {
                        Value = None; Operation = "*";
                        Left = Some(
                        { 
                            Value = None; Operation = "/"; 
                            Left = Some({ Value = Some(1.); Operation = ""; Left = None; Right = None; }); 
                            Right = node.Right; 
                        });
                        Right = Some(derivativeFunc node.Right.Value)
                    }
    | "sin" -> // sin(x)
                if node.Right.Value.Operation = "x" then
                    { Value = None; Operation = "cos"; Left = None; Right = node.Right; }
                // sin(f(x))
                else
                    {
                        Value = None; Operation = "*";
                        Left = Some(derivativeFunc node.Right.Value);
                        Right = Some({ Value = None; Operation = "cos"; Left = None; Right = node.Right; });
                    }
    | "cos" -> // cos(x)
                if node.Right.Value.Operation = "x" then
                    {
                        Value = None; Operation = "-";
                        Left = Some({ Value = Some(0.); Operation = ""; Left = None; Right = None; });
                        Right = Some({ Value = None; Operation = "sin"; Left = None; Right = node.Right; });
                    }
                // cos(f(x))
                else
                    {
                        Value = None; Operation = "*";
                        Left = Some(derivativeFunc node.Right.Value);
                        Right = Some(
                        {
                            Value = None; Operation = "-";
                            Left = Some({ Value = Some(0.); Operation = ""; Left = None; Right = None; });
                            Right = Some({ Value = None; Operation = "sin"; Left = None; Right = node.Right; });
                        });
                    }
    | _ -> failwith "Unknown operation"

