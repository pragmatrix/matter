module Syntax

open Tokenizer

type State = { Base : int option; Level : int }

// this is supposed to consume all Newline and Indent tokens!
let lightSyntax tokens =
    let ends num = List.replicate num End

    let rec p res rest state = 
        match rest with

        | Newline :: Indent num :: Comma :: rest ->
            p res rest state

        | Newline :: Indent num :: rest ->
            let b = state.Base
            match b with
            | None ->
                p (Begin :: res) rest { state with Base = Some num; Level = 0 }
            | Some b ->
                let indent = b + state.Level
                match num with
                | _ when num = indent ->
                    p (Begin :: End :: res) rest state
                | _ when num = indent+1 ->
                    p (Begin :: res) rest { state with Level = state.Level+1 }
                | _ when num < indent && num >= b ->
                    let diff = indent - num
                    p (Begin :: (ends (diff+1) @ res)) rest { state with Level = num - b }
                | _ -> 
                    failwith (sprintf "Indentation change from %d to %d is not supported" indent num)


        | Newline :: rest -> 
            p res (Newline :: Indent 0 :: rest) state

        | t :: rest ->
            p (t :: res) rest state

        | [] -> 
            if state.Base = None then
                []
            else
                let indents = state.Level + 1
                (ends indents) @ res

    // remove empty lines and convert Newline Indent pairs to Indent only

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
                
    p [] cleaned { Base = None; Level = 0 } |> List.rev

let braceSyntax tokens =
    let tokenFilter t = 
        match t with
        | Newline -> false
        | Indent _ -> false
        | Comma -> false
        | _ -> true
    
    List.filter tokenFilter tokens
