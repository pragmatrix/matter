module Syntax

open Tokenizer

type State = { Base : int option; Level : int }

// this is supposed to consume all Newline and Indent tokens!
let indentSyntax tokens =
    let ends num = List.replicate num End

    // add braces depending indent and preserve newlines for postprocessing
    // todo: better to feed only Indent here?

    let rec brace res rest state = 
        match rest with

        | Newline :: Indent num :: Comma :: rest ->
            brace (Newline::res) rest state

        | Newline :: Indent num :: rest ->
            let b = state.Base
            match b with
            | None ->
                brace (Begin :: Newline :: res) rest { state with Base = Some num; Level = 0 }
            | Some b ->
                let indent = b + state.Level
                match num with
                | _ when num = indent ->
                    brace (Begin :: Newline :: End :: res) rest state
                | _ when num = indent+1 ->
                    brace (Begin :: Newline :: res) rest { state with Level = state.Level+1 }
                | _ when num < indent && num >= b ->
                    let diff = indent - num
                    brace (Begin :: Newline :: (ends (diff+1) @ res)) rest { state with Level = num - b }
                | _ -> 
                    failwith (sprintf "Indentation change from %d to %d is not supported" indent num)

        // convert single Newline to Indent 0
        | Newline :: rest -> 
            brace res (Newline :: Indent 0 :: rest) state

        | t :: rest ->
            brace (t :: res) rest state

        | [] -> 
            if state.Base = None then
                []
            else
                let indents = state.Level + 1
                (ends indents) @ res

    let brace tokens = brace [] tokens { Base = None; Level = 0 } |> List.rev
    // remove empty lines

    let rec cleanup res rest =
        match rest with
        | Newline :: Indent _ :: Newline :: rest ->
            cleanup res (Newline :: rest)

        | Newline :: Newline :: rest ->
            cleanup res (Newline :: rest)

        | Newline :: Indent _ :: [] ->
            res

        | Newline :: [] ->
            res
            
        | t :: rest ->
            cleanup (t::res) rest

        | [] -> res


    let cleaned = cleanup [] tokens |> List.rev

    // add a newline to begin with
    let cleaned = 
        match cleaned with
        | Newline :: rest ->
            cleaned
        | _ -> 
            Newline :: cleaned

    // post process
    // - replace single line (($)) with ($) only
    // - replace single line ($) with $ only
    // - remove all newlines

    let rec post res rest =
        match rest with
        | Newline :: Begin :: Begin :: tk :: End :: End :: r ->
            match r with 
            | Newline :: res -> post (End :: tk :: Begin :: res) r
            | [] -> post (End :: tk :: Begin :: res) r
            | _ -> post res (tail rest)

        | Newline :: Begin :: tk :: End :: r ->
            match r with
            | Newline :: rest -> post (tk :: res) r
            | [] -> post (tk :: res) r
            | _ -> post res (tail rest)

        | Newline :: r -> post res r
        | t::r -> post (t :: res) r
        | [] -> res

    let post tokens = post [] tokens |> List.rev
                
    brace cleaned |> post

let braceSyntax tokens =
    let tokenFilter t = 
        match t with
        | Newline -> false
        | Indent _ -> false
        | Comma -> false
        | _ -> true
    
    List.filter tokenFilter tokens
