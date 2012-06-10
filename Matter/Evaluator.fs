﻿module Evaluator

open Expression
open Parser

type Env = Map<string, Function >

let ok = Keyword "ok"

let (|IsAtomic|_|) expression =
    let yes = Some expression
    match expression with
    | Number _ -> yes
    | String _ -> yes
    | Boolean _ -> yes
    | Keyword _ -> yes
    | _ -> None

let rec bind parms expressions (env:Env) =
    match parms, expressions with
    | [],[] -> env
    | Symbol sym:: parm_r, value :: value_r ->
        let newEnv = env.Add(sym, makeVar sym value)
        bind parm_r value_r newEnv
    | _ -> failwith "bind: failed to bind expressions to arguments"



let rec eval expression (env:Env) =
    match expression with
    | IsAtomic exp -> exp, env

    // variable
    | Symbol s -> 
        let f = env.[s]
        evalFun f [] env

    // special forms and application
    | List (Symbol s::parms) -> 
        match s with
        | "do" -> evalDo parms env
        | "def" -> evalDef parms env
        | "defmacro" -> evalDefmacro parms env
        | "if" -> evalIf parms env
        | _ -> 
            let f = env.[s]
            evalFun f parms env

    | _ -> failwith "failed to evaluate expression"

and evalDo expressions env =
    List.fold (fun (_,env) exp -> eval exp env) (List [], env) expressions
    
and evalDef parms (env:Env) =
    
    let def symbol parms body =
        let f args = 
            let localEnv = bind parms args env
            eval body localEnv |> fst
        ok, env.Add(symbol, makeFunction symbol f)

    match parms with
    | [Symbol symbol; body] -> def symbol [] body
    | [Symbol symbol; List parms; body] -> def symbol parms body

    | _ -> failwith "def: invalid arguments"

and evalDefmacro parms (env:Env) =

    let def symbol parms body = 
        let f args = 
            let localEnv = bind parms args env
            eval body localEnv |> fst
        ok, env.Add(symbol, makeMacro symbol f)

    match parms with
    | [Symbol symbol; body] -> def symbol [] body
    | [Symbol symbol; List parms; body] -> def symbol parms body

    | _ -> failwith "defmacro: invalid arguments"

and evalIf parms (env:Env) =
    match parms with
    | [test; ifTrue; ifFalse] ->
        let value = evalValue test env
        match value with
        | Boolean b -> eval (if b then ifTrue else ifFalse) env
        | _ -> failwith "if: expect boolean expression"

    | [test; ifTrue ] ->
        let value = evalValue test env
        match value with
        | Boolean true -> eval ifTrue env
        | Boolean false -> List [], env
        |_ -> failwith "if: expect boolean expression"

    | _ -> failwith "if: (if exp then else?)"

and evalFun f args (env:Env) =
    if (not f.Macro) then
        (f.Eval args), env
    else
        eval (f.Eval args) env

and evalValue exp env = eval exp env |> fst

let evaluateString str =
    let expressions = parseString str
    let program = (List (Symbol "do" :: expressions))
    eval program Map.empty |> fst
