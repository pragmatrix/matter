module Evaluator

open Expression
open Parser
open Runtime


let ok = Keyword "ok"

let (|IsSelfEval|_|) expression =
    let yes = Some expression
    match expression with
    | Number _ -> yes
    | String _ -> yes
    | Boolean _ -> yes
    | Keyword _ -> yes
    | _ -> None

// returns the result of the lookup and the frame it was found in

let rec eval expression (frame:Frame) =
    match expression with
    | IsSelfEval exp -> exp, frame

    // variable (evaluates to the "value" of the symbol)
    | Symbol s ->
        let r = Frame.lookup s frame
        match r with
        | None -> failwith (sprintf "undefined symbol '%s'" s)
        | Some (exp, fframe) ->
            match exp with
            // right now we "evaluate" the value on lookup.
            | Var { Value=value } -> (value fframe), frame
            | Func f -> ResolvedFunc(fframe, f), frame
            // todo: do macros have a value?
            | _ -> exp, frame


    // special forms and function application
    | List (operator::args) -> 
        match operator with
        | Symbol "do" -> evalDo args frame
        | Symbol "defmacro" -> evalDefmacro args frame
        | Symbol "if" -> evalIf args frame
        | Symbol "quote" -> evalQuote args frame
        | Symbol "." -> evalDot args frame
        | _ -> 
            let finalOperator = eval operator frame |> fst
            match finalOperator with
            | Macro (m) -> 
                // a macro is not allowed to pollute our current environment, but
                // it can have an effect on it by returning defs or other defmacros.
                let macroExp, envMacro = evalMacro m args frame
                eval macroExp frame
            | ResolvedFunc (fframe, f) ->
                let args = evalArgs args frame
                evalFun f args fframe, frame
            | _ -> failwith (sprintf "expect function, but seen %s" (print finalOperator))

    | _ -> failwith "failed to evaluate expression"

and evalArgs args frame =
    List.map (fun exp -> eval exp frame |> fst) args

and evalDo expressions frame =
    
    let analyzeDef name parms body frame =
        let isValue = List.isEmpty parms
        // env of a function is lexically scoped!
        let f fframe args =
            let lframe = bind parms args fframe
            eval body lframe |> fst

        let exp = 
            if isValue then 
                makeVar name (fun fframe -> f fframe []) 
            else 
                makeFunction name f

        Frame.add frame (name, exp)

    let analyzeDef expression = 
        match expression with
        | List [Symbol("def") ; Symbol name ; body] -> analyzeDef name [] body
        | List [Symbol("def") ; Symbol name ; List parms; body ] -> analyzeDef name parms body
        | _ -> failwith "def: invalid arguments"

    let isDef expression =
        match expression with
        | List (Symbol("def") :: _) -> true
        | _ -> false

    let (defs, rest) = List.partition isDef expressions

    let analyzedDefs = List.map analyzeDef defs

    // all defs together get their own frame.
    let defsFrame = List.fold (fun f a -> a f) (Frame.derive frame) analyzedDefs

    List.fold (fun (_,doframe) exp -> eval exp doframe) (List [], defsFrame) rest

and evalDefmacro parms frame =

    let def symbol parms body = 
        let exp = makeMacro symbol parms body
        ok, Frame.add frame (symbol, exp)

    match parms with
    | [Symbol symbol; body] -> def symbol [] body
    | [Symbol symbol; List parms; body] -> def symbol parms body

    | _ -> failwith "defmacro: invalid arguments"

and evalIf parms frame =
    match parms with
    | [test; ifTrue; ifFalse] ->
        let value = evalValue test frame
        match value with
        | Boolean b -> eval (if b then ifTrue else ifFalse) frame
        | _ -> failwith "if: expect boolean expression"

    | [test; ifTrue ] ->
        let value = evalValue test frame
        match value with
        | Boolean true -> eval ifTrue frame
        | Boolean false -> List [], frame
        |_ -> failwith "if: expect boolean expression"

    | _ -> failwith "if: (if exp then else?)"

and evalQuote parms env =
    match parms with
    | p :: [] -> p,env
    | _ -> failwith "quote expects only one parameter"

and evalFun f args fframe =
    (f.F fframe args)
    
and evalMacro m args frame =
    let localEnv = bind m.Parms args frame
    eval m.Body localEnv

and bind parms (args:Expression list) (frame:Frame) =
        match parms, args with
        | [],[] -> frame
        | Symbol sym:: parm_r, value :: value_r ->
            let newFrame = Frame.add frame (sym, makeVar sym (fun _ -> value))
            bind parm_r value_r newFrame
        | _ -> failwith "bind: failed to bind expressions to arguments"

and evalValue exp env = 
    eval exp env |> fst

and evalDot args env =
    match args with
    | [Symbol name] ->
        match Map.tryFind name functionMap with
        | Some f -> f, env
        | None -> failwith (sprintf ". %s not implemented" name)
    | _ -> failwith ". expects one symbol"

//

let doify expressions = 
    (List (Symbol "do" :: expressions))

let evalExpressions expressions = 
    eval (doify expressions) Frame.empty

let evalString syntax str = 
    let expressions = parseString syntax str
    evalExpressions expressions

