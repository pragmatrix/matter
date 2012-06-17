module Syntax

open Tokenizer

type State = { AnyTokens : bool; Indent : int }

// this is supposed to consume all Newline and Indent tokens!
let lightSyntax tokens =
    let ends num = List.replicate num End

    let rec p res rest state = 
        match rest with
        | Newline :: Indent num :: rest ->
            let current = state.Indent
            match num with
            | _ when num = current ->
                p (End :: Begin :: res) rest state
            | _ when num = current+1 ->
                p (Begin :: res) rest { state with Indent = num }
            | _ when num < current ->
                let diff = current - num
                p ((ends diff) @ (Begin :: res)) rest { state with Indent = num }
            | _ -> 
                failwith (sprintf "Indentation change from %d to %d is not supported" current num)

        | Newline :: rest -> 
            if state.AnyTokens then
                // not seen any tokens yet
                p (End :: res) rest { state with AnyTokens = false }
            else
                p res rest state

        | t :: rest ->
            match state.AnyTokens with
            | false ->
                // remember we push stuff out in reverse, so token comes first
                p (t :: Begin :: res) rest { state with AnyTokens = true }
            | true ->
                p (t :: res) rest state
        | [] -> 
            let indents = state.Indent + (if state.AnyTokens then 1 else 0)
            (ends indents) @ res

    p [] tokens { AnyTokens =false; Indent = 0 } |> List.rev

let braceSyntax tokens =
    let tokenFilter t = 
        match t with
        | Newline -> false
        | Indent _ -> false
        | _ -> true
    
    List.filter tokenFilter tokens
