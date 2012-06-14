module Evaluator

open Expression
open Parser

type Env = Map<string, Expression >

let ok = Keyword "ok"

let (|IsSelfEval|_|) expression =
    let yes = Some expression
    match expression with
    | Number _ -> yes
    | String _ -> yes
    | Boolean _ -> yes
    | Keyword _ -> yes
    | _ -> None

let rec eval expression (env:Env) =
    match expression with
    | IsSelfEval exp -> exp, env

    // variable (evaluates to the "value" of the symbol)
    | Symbol s ->
        let exp = env.TryFind s
        match exp with
        | None -> failwith (sprintf "undefined symbol '%s'" s)
        | Some exp ->
            match exp with
            | Var { Value=value } -> value, env
            // todo: do macros have a value?
            | _ -> exp, env


    // special forms and function application
    | List (operator::args) -> 
        match operator with
        | Symbol "do" -> evalDo args env
        | Symbol "def" -> evalDef args env
        | Symbol "defmacro" -> evalDefmacro args env
        | Symbol "if" -> evalIf args env
        | Symbol "quote" -> evalQuote args env
        | Symbol "." -> evalDot args env
        | _ -> 
            let finalOperator = eval operator env |> fst
            match finalOperator with
            | Macro (m) -> 
                // a macro is not allowed to pollute our current environment, but
                // it can have an effect on it by returning defs or other defmacros.
                let macroExp, envMacro = evalMacro m args env
                eval macroExp env
            | Func (f) ->
                let args = evalArgs args env
                evalFun f args env
            | _ -> failwith "expect function, but seen %s" (print finalOperator)

    | _ -> failwith "failed to evaluate expression"

and apply operator args env =
    match operator with
    | Func f -> evalFun f args env
    | _ -> failwith (sprintf "apply: undefined %s" (print operator))

and evalArgs args env =
    List.map (fun exp -> eval exp env |> fst) args

and evalDo expressions env =
    List.fold (fun (_,env) exp -> eval exp env) (List [], env) expressions
    
and bind parms args (env:Env) =
        match parms, args with
        | [],[] -> env
        | Symbol sym:: parm_r, value :: value_r ->
            let newEnv = env.Add(sym, makeVar sym value)
            bind parm_r value_r newEnv
        | _ -> failwith "bind: failed to bind expressions to arguments"

and evalDef parms (env:Env) =
    
    let def name parms body =
        let isValue = List.isEmpty parms
        // env of a function is lexically scoped!
        let f args =
            let localEnv = bind parms args env
            eval body localEnv |> fst

        let exp = 
            if isValue then 
                makeVar name (f []) 
            else 
                makeFunction name f

        ok, env.Add(name, exp)


    match parms with
    | [Symbol symbol; body] -> def symbol [] body
    | [Symbol symbol; List parms; body] -> def symbol parms body

    | _ -> failwith "def: invalid arguments"

and evalDefmacro parms (env:Env) =

    let def symbol parms body = 
        ok, env.Add(symbol, makeMacro symbol parms body)

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

and evalQuote parms env =
    match parms with
    | p :: [] -> p,env
    | _ -> failwith "quote expects only one parameter"

and evalFun f args (env:Env) =
    (f.F args), env
    
and evalMacro m args (env:Env) =
    let localEnv = bind m.Parms args env
    eval m.Body localEnv

and evalValue exp env = 
    eval exp env |> fst

and evalDot args env =
    match args with
    | [Symbol "list"] ->
        makeFunction "list" List, env
    | [Symbol n] -> failwith (sprintf ". %s not implemented" n)
    | _ -> failwith ". expects a symbol as its only argument"

//

let doify expressions = 
    (List (Symbol "do" :: expressions))

let evalExpressions expressions = 
    eval (doify expressions) Map.empty

let evalString str = 
    let expressions = parseString str
    evalExpressions expressions

