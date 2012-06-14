module Expression


type Expression =
    // atoms:

    | Number of int
    // | Float of float
    // | Character of char
    | String of string
    | Boolean of bool
    | Keyword of string 
    | Symbol of string

    | List of Expression list

    | Func of Function
    | Var of Variable


and Function = { Name: string; Parms: Expression list; Body: Expression; Macro: bool }
and Variable = { Name: string; Value: Expression }

let makeFunction name parms body =
    Func { Name = name; Parms = parms; Body = body; Macro = false }

let makeVar name value =
    Var { Name = name; Value = value }

let makeMacro name parms body =
    Func { Name = name; Parms = parms; Body = body; Macro = true }

let rec print exp =
    match exp with
    | Number n -> n.ToString()
    | String str -> "\"" + str + "\""
    | Boolean b -> if b then "true" else "false"
    | Keyword kw -> ":" + kw
    | Symbol s -> s
    | List lst ->
        let all = List.map print lst
        let content = List.fold (fun str next -> if (str = "") then next else str + " " + next) "" all
        "("+content+")"
    | Func {Name = name} -> "fun " + name
    | Var { Name = name} -> "var " + name
