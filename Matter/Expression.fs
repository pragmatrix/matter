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

and Function = { Name: string; Eval: Expression list -> Expression; Macro: bool }

let makeFunction name eval =
    { Name = name; Eval = eval; Macro = false }

let makeVar name value =
    makeFunction name (fun _ -> value)

let makeMacro name eval =
    { Name = name; Eval = eval; Macro = true }

let rec print exp =
    match exp with
    | Number n -> n.ToString()
    | String str -> "\"" + str + "\""
    | Boolean b -> if b then "true" else "false"
    | Keyword kw -> ":" + kw
    | Symbol s -> s
    | List lst ->
        let all = List.map print lst
        let content = List.fold (fun str next -> str + " " + next) "" all
        "("+content+")"
    | Func {Name = name} -> "call " + name


