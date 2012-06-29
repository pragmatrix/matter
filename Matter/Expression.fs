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

    | Lambda of (Expression list -> Expression)
    | Function of (Frame -> Expression list -> Expression)
    | Variable of (Frame -> Expression)
    | Macro of Macro

and Macro =    
    { Name: string; Parms: Expression list; Body: Expression }
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
    | Lambda _ -> "fun "
    | Function _ -> "function "
    | Variable _ -> "var "
    | Macro { Name = name } -> "macro " + name

let doify expressions = 
    (List (Symbol "do" :: expressions))