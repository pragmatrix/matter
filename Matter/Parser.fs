module Parser

open Tokenizer


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


let atom token = 
    match token with
    | Tokenizer.Number n -> Number n
    | Tokenizer.String str -> String str
    | Tokenizer.Boolean bool -> Boolean bool
    | Tokenizer.Keyword kw -> Keyword kw
    | Tokenizer.Symbol s -> Symbol s
    | _ -> failwith "expected atom"


let parse tokens =
    let rec parseInner res tokens =
        match tokens with
        | [] -> 
            res, []
        | End :: rest ->
            res, rest
        | Begin :: rest ->
            let nested, rest = update_res (reverse >> List) (parseInner [] rest)
            let res = nested :: res
            parseInner res rest
        | t :: rest -> 
            let res = atom t :: res
            parseInner res rest
          
    let res, leftover = parseInner [] tokens
    match leftover with
    | [] -> reverse res
    | _ -> failwith "Failed to completely parse input"

let parseString str =
    tokenizeString str |> parse
