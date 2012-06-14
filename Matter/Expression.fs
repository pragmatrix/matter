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
    | Macro of Macro


and Function = 
    { Name: string; F: Expression list -> Expression }
and Macro =    
    { Name: string; Parms: Expression list; Body: Expression }
and Variable = 
    { Name: string; Value: Expression }

let makeFunction name f =
    Func { Name = name; F = f }

let makeVar name value =
    Var { Name = name; Value = value }

let makeMacro name parms body =
    Macro { Name = name; Parms = parms; Body = body }

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
    | Func { Name = name } -> "fun " + name
    | Var { Name = name } -> "var " + name
    | Macro { Name = name } -> "macro " + name

