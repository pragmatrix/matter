module Tokenizer

open System

type Token = 
    // literals:

    | Number of int
    // | Float of float
    // | Character of char
    | String of string
    | Boolean of bool
    | Keyword of string 

    | Symbol of string

    | Begin
    | End
    | Quote

    | Newline
    | Indent of int 
    | Comma


let rec takeWhileCore f (s,s2) = 
    match s2 with
    | c :: rest when f c -> 
        takeWhileCore f (c::s, rest)
    | _ -> (s, s2)

let takeWhile f input =
    takeWhileCore f ([], input)
      
let skip f input =
    match input with
    | [] -> []
    | v :: rest ->
        if (f v) then rest else input

let isAlpha c = (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
let isDigit c = (c >= '0' && c <= '9')

// right now we support '.' in symbols .. that may change later
let isSymbolSpecial (c:char) = "!?_+-*/.".IndexOf(c) <> -1

let inline (>>||) a b = fun c -> (a c) || (b c)

let isSymbolFirst = isAlpha >>|| isSymbolSpecial
let isSymbolRest = isSymbolFirst >>|| isDigit

let str (chars: char list) =
    String.Concat(Array.ofList(chars))
let reverse = List.rev
let revstr = reverse >> str

let tail = List.tail
let head = List.head

let update_fst f (a,b) = (f a, b)
let update_snd f (a,b) = (a, f b)

let update_res = update_fst

let takeSymbol input =

    let s, rest = takeWhile isSymbolRest (tail input)
    let symbol = (head input) :: (reverse s)
    (str symbol), rest 
      
let takeString input = 

    let rec takeStringRest str input =
        match input with
        | '"' :: rest -> str, rest
        | c :: rest -> 
            takeStringRest (c::str) rest
        | [] -> str, []

    match input with
    | '"'::rest -> takeStringRest [] rest |> update_res revstr
    | _ -> failwith "invalid string"


let takeNumber = takeWhile isDigit

let isWhitespace c = 
    c = ' ' || c = '\t' || c = '\r'

let rec tokenize res input =
    match input with
    | [] -> res

    | '"' :: rest ->
        let str, rest = takeString input
        let literal = String str
        let res = literal :: res
        tokenize res rest

    | ':' :: rest ->
        let symbol, rest = takeSymbol rest
        let res = (symbol |> Keyword) :: res
        tokenize res rest

    | '(' :: rest ->
        let res = Begin :: res
        tokenize res rest

    | ')' :: rest ->
        let res = End :: res
        tokenize res rest

    | '\'' :: rest ->
        let res = Quote :: res
        tokenize res rest

    | ';' :: rest ->
        let rest = skip ( (<>) '\n') rest
        tokenize res rest

    | '\n' :: rest ->
        let res = Newline :: res
        tokenize res rest

    | '\t' :: rest ->
        let tabs, rest = takeWhile ((=) '\t') rest
        let res = Indent (tabs.Length+1) :: res
        tokenize res rest

    | ',' :: rest ->
        let res = Comma :: res
        tokenize res rest
        
    | c :: rest ->
        match c with
        | c when isSymbolFirst c ->
            let symbol, rest = takeSymbol input
            let r = 
                match symbol with
                | "true" -> Boolean true
                | "false" -> Boolean false
                | _ -> Symbol symbol
            let res = r :: res
            tokenize res rest

        | c when isDigit c ->
            let number, rest = takeNumber input
            let res = (Number (Int32.Parse( number |> revstr))) :: res
            tokenize res rest

        | c when isWhitespace c ->
            tokenize res rest

        | _ -> failwith (sprintf "failed to tokenize '%c'" c)


let tokenizeString str : Token list = 
    str |> Seq.toList |> tokenize [] |> reverse








