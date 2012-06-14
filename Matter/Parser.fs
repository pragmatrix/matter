module Parser

open Tokenizer
open Expression

let (|Atom|_|) token = 
    match token with
    | Tokenizer.Number n -> Some (Number n)
    | Tokenizer.String str -> Some (String str)
    | Tokenizer.Boolean bool -> Some (Boolean bool)
    | Tokenizer.Keyword kw -> Some (Keyword kw)
    | Tokenizer.Symbol s -> Some(Symbol s)
    | _ -> None
    

let parse tokens =
    let rec parseInner res tokens =
        match tokens with
        | [] -> 
            res, []
        | End :: _ ->
            // never consume the end (must be done at begin)
            res, tokens
        | Begin :: rest ->
            let nested, rest = update_res reverse (parseInner [] rest)
            match rest with
            | End :: rest ->
                let res = (List nested) :: res
                parseInner res rest
            | _ -> failwith "missing ')'"

        | Quote :: rest ->
            let tail, rest = update_res reverse (parseInner [] rest)
            match tail with
            | quoted :: tailq ->
                let resq = (reverse tailq) @ (List [(Symbol "quote") ; quoted] :: res)
                parseInner resq rest
            | _ -> failwith "' must be followed by expression"
        | Atom t :: rest -> 
            let res = t :: res
            parseInner res rest
        | _ -> failwith "failed to tokenize input"
          
    let res, leftover = parseInner [] tokens
    match leftover with
    | [] -> reverse res
    | _ -> failwith "Failed to completely parse input"

let parseString str =
    tokenizeString str |> parse
