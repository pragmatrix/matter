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
    | Macro of (Expression list -> Expression)

let rec equal l r = 
    match (l, r) with
    | Number l, Number r -> l = r
    | String l, String r -> l = r
    | Boolean l, Boolean r -> l = r
    | Keyword l, Keyword r -> l = r
    | Symbol l, Symbol r -> l = r
    | List l, List r -> 
        if ((List.length l) <> (List.length r)) then
            false
        else
            let zipped = List.zip l r
            List.forall (fun (l, r) -> equal l r) zipped
    | _ -> false

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
    | Macro _ -> "macro "

let doify expressions = 
    (List (Symbol "do" :: expressions))

type Frame = 
    | Frame of Frame option * Map<string, Record>

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

and Record =
    | Function of (Frame -> Expression list -> Expression)
    | Variable of (Frame -> Expression)
    | Macro of (Frame -> Expression list -> Expression)

