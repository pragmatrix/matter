module Evaluator

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
    | Symbol s -> env.[s].Eval [], env

    // special forms and application
    | List (Symbol s::parms) -> 
        match s with
        | "begin" -> evalBegin parms env
        | "define" -> evalDefine parms env
        | "if" -> evalIf parms env
        | _ -> env.[s].Eval parms, env

    | _ -> failwith "failed to evaluate expression"

and evalBegin expressions env =
    List.fold (fun (_,env) exp -> eval exp env) (List [], env) expressions
    
and evalDefine parms (env:Env) =
    match parms with

    | [Symbol symbol; body] ->
        let f = makeVar symbol body
        ok, env.Add(symbol, f)

    | [Symbol symbol; List parms; body] ->
        let f args = 
            let localEnv = bind parms args env
            eval body localEnv |> fst
        ok, env.Add(symbol, makeFunction symbol f)

    | _ -> failwith "define: invalid arguments"

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

and evalValue exp env = eval exp env |> fst

let evaluateString str =
    let expressions = parseString str
    let program = (List (Symbol "begin" :: expressions))
    eval program Map.empty |> fst
