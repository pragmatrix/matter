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
    | ResolvedFunc of Frame * Function
    | Var of Variable
    | Macro of Macro
    | Lambda of (Expression list -> Expression)

and Function = 
    { Name: string; F: Frame -> Expression list -> Expression }
and Macro =    
    { Name: string; Parms: Expression list; Body: Expression }
and Variable = 
    { Name: string; Value: Frame -> Expression }
and Frame = 
    | Frame of Frame option * Map<string, Expression>

    static member empty = Frame(None, Map.empty)

    static member derive parent = 
        Frame (Some parent, Map.empty)

    static member add (Frame(parent, map)) v =
        let newMap = map.Add v
        Frame(parent, newMap)

    static member lookup str (Frame(parent, current) as frame) =
        let r = Map.tryFind str current
        match r with
        | Some r -> Some (r, frame)
        | None ->
            match parent with
            | Some p -> Frame.lookup str p
            | None -> None


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
    | ResolvedFunc(frame, f) -> "resolved " + (print (Func f))
    | Var { Name = name } -> "var " + name
    | Macro { Name = name } -> "macro " + name
    | Lambda _ -> "fun "

let doify expressions = 
    (List (Symbol "do" :: expressions))