module Evaluator

open Parser

type Env = Map<string, Expression list ->Expression >

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
        let newEnv = env.Add(sym, fun _ -> value)
        bind parm_r value_r newEnv
    | _ -> failwith "bind: failed to bind expressions to arguments"

let rec eval expression (env:Env) =
    match expression with
    | IsAtomic exp -> exp, env

    // variable
    | Symbol s -> env.[s] [], env

    // application
    | List (Symbol s::parms) -> 
        match s with
        | "begin" -> evalBegin parms env
        | "define" -> evalDefine parms env
        | _ -> env.[s] parms, env

    | _ -> failwith "failed to evaluate expression"

and evalBegin expressions env =
    List.fold (fun (_,env) exp -> eval exp env) (List [], env) expressions
    
and evalDefine parms (env:Env) =
    match parms with

    | [Symbol symbol; body] ->
        ok, env.Add(symbol, fun _ -> body)

    | [Symbol symbol; List parms; body] ->
        let f args = 
            let localEnv = bind parms args env
            eval body localEnv |> fst
        ok, env.Add(symbol, f)

    | _ -> failwith "define: invalid arguments"

let evaluateString str =
    let expressions = parseString str
    let program = (List (Symbol "begin" :: expressions))
    eval program Map.empty |> fst
