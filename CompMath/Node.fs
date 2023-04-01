module Node

exception UnknownOperation of string

// 0000 - 009F
let FUNCS_MAP =
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

let FUNCS = Map.toArray FUNCS_MAP |> Array.map snd

let ALLOWED_ARGS = [| 'a'; 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'i'; 'g'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z' |]

let constsMap = 
    Map [ (""  , '\u0000')
          ("pi", '\u0001')
          ("e" , '\u0002')]

let CONSTS =  Map.toArray constsMap |> Array.map snd

let OPERATIONS = [| '+'; '-'; '*'; '/'; '^' |]

let SYMBOLS = Array.append OPERATIONS [|'('; '|'|]

type Operation =
    | Dec = -1
    | Inc = 1

type Node =
    { Value: float
      Operation: char
      Left: Option<Node>
      Right: Option<Node> }
    
    member this.ToMathJax() =
        let summary = 
            [||] 
            |> Array.append CONSTS
            |> Array.append ALLOWED_ARGS
            |> Array.append FUNCS
        
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
                Array.contains val1 ALLOWED_ARGS ->  
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
            |> Array.append CONSTS
            |> Array.append ALLOWED_ARGS
            |> Array.append FUNCS
        
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
            && operation <> '^'
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
                elif
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
                            || node.Right.Value.Operation = '*'
                            || node.Right.Value.Operation = '/'

                if
                    left && right
                then
                    "(" + convert2str node.Left.Value + ")/(" + convert2str node.Right.Value + ")"
                elif
                    left
                then
                    "(" + convert2str node.Left.Value + ")/" + convert2str node.Right.Value
                elif 
                    right
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
                Array.contains val1 ALLOWED_ARGS ->  
                    string val1
            | val1 when Array.contains val1 FUNCS ->
                Map.findKey (fun _ v -> v = val1) FUNCS_MAP + "(" + convert2str node.Right.Value + ")"
            | _ -> raise (UnknownOperation (string node.Operation))

        convert2str this

let isComplexFunction (node: Node) = Array.contains node.Operation FUNCS

let isConst (node: Node) = Array.contains node.Operation CONSTS

let isArg (node: Node) = Array.contains node.Operation ALLOWED_ARGS

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
      Operation = (Map.find func FUNCS_MAP)
      Left = None
      Right = Some node }

let combine (op: char) (left: Node) (right: Node) =
    { Value = 0
      Operation = op
      Left = Some left
      Right = Some right }
