﻿module Evaluator

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

let rec eval expression (frame:Frame) =
    match expression with
    | IsSelfEval exp -> exp, frame

    | Symbol s ->
        let r = Frame.lookup s frame
        match r with
        | Some (exp, fframe) ->
            // function, variables are bound to their frame
            let v = 
                match exp with
                | Variable f -> (f fframe)
                | Function f -> Lambda (f fframe)
                // todo: clarify frame for macros
                | Macro m -> Expression.Macro (m fframe)
            v, frame
        | None -> failwith (sprintf "undefined symbol '%s'" s)

    // special forms and function application
    | List (operator::args) -> 
        match operator with
        | Symbol "fun" -> evalFun args frame, frame

        | Symbol "do" -> evalDo args frame
        | Symbol "defmacro" -> evalDefmacro args frame

        | Symbol "if" -> evalIf args frame
        // defs can appear here to (not only inside do, for example in an if-then-else)
        | Symbol "def" -> evalDef args frame
        // could be replaced by a def generating macro, because defs in macros have no simultaneous scope.
        | Symbol "let" -> evalDef args frame
        | Symbol "quote" -> evalQuote args, frame
        | Symbol "." -> evalDot args, frame
        | _ -> 
            let finalOperator = eval operator frame |> fst
            match finalOperator with
            | Lambda f ->
                let args = evalArgs args frame
                f args, frame
            | Expression.Macro m -> 
                // a macro itself not allowed to pollute our current environment, but
                // it can have an effect on it by returning defs or other defmacros.
                eval (m args) frame
            | _ -> failwith (sprintf "expect lambda or macro, but seen %s" (print finalOperator))

    | _ -> failwith "failed to evaluate expression"

and evalArgs args frame =
    List.map (fun arg -> eval arg frame |> fst) args

and evalFun expressions frame =
    let lambda symbols body =
        let apply args =
            let lframe = bind symbols args frame
            eval body lframe |> fst
        (Lambda apply)

    match expressions with
    | Symbol parm :: body -> lambda [Symbol(parm)] (doify body)
    | List symbols :: body -> lambda symbols (doify body)
    | _ -> failwith "fun: invalid arguments"

and analyzeDef args = 
    let analyzeDef name parms body frame =
        let isValue = List.isEmpty parms
        // frame of a function is lexically scoped!
        let apply fframe args =
            let lframe = bind parms args fframe
            eval body lframe |> fst

        let record = 
            if isValue then 
                Variable (fun fframe -> apply fframe []) 
            else 
                Function apply

        Frame.add frame (name, record)

    match args with
    | [Symbol name ; body] -> analyzeDef name [] body
    | [Symbol name ; List parms; body ] -> analyzeDef name parms body
    | _ -> failwith "def: invalid arguments"

and evalDef args frame =
    ok, analyzeDef args frame

and evalDo expressions frame =

    let isDef expression =
        match expression with
        | List (Symbol("def") :: _) -> true
        | _ -> false

    let (defs, rest) = List.partition isDef expressions

    let defArgs (List l) = List.tail l

    let analyzedDefs = List.map (defArgs >> analyzeDef) defs

    // all defs together get their own frame.
    let defsFrame = List.fold (fun f a -> a f) (Frame.derive frame) analyzedDefs

    List.fold (fun (_,doframe) exp -> eval exp doframe) (List [], defsFrame) rest

and evalDefmacro parms frame =

    let def symbol parms body = 

        let evalMacro frame args =
            let localEnv = bind parms args frame
            eval body localEnv |> fst

        let record = Macro evalMacro
        ok, Frame.add frame (symbol, record)

    match parms with
    | [Symbol symbol; body] -> def symbol [] body
    | [Symbol symbol; List parms; body] -> def symbol parms body

    | _ -> failwith "defmacro: invalid arguments"

and evalIf parms frame =
    
    let evalValue exp env = 
        eval exp env |> fst

    match parms with
    | [predicate; ifTrue; ifFalse] ->
        let value = evalValue predicate frame
        match value with
        | Boolean b -> eval (if b then ifTrue else ifFalse) frame
        | _ -> failwith "if: expect boolean expression"

    | [predicate; ifTrue ] ->
        let value = evalValue predicate frame
        match value with
        | Boolean true -> eval ifTrue frame
        | Boolean false -> List [], frame
        |_ -> failwith "if: expect boolean expression"

    | _ -> failwith "if: (if exp then else?)"

and evalQuote args =
    match args with
    | p :: [] -> p
    | _ -> failwith "quote expects only one parameter"

and bind symbols (args:Expression list) (frame:Frame) =
        match symbols, args with
        | [],[] -> frame
        | Symbol sym:: parm_r, value :: value_r ->
            let newFrame = Frame.add frame (sym, Variable (fun _ -> value))
            bind parm_r value_r newFrame
        | _ -> failwith "bind: failed to bind expressions to arguments"

// '.', the rabbit hole ;)

and evalDot args =
    match args with
    | [Symbol name] ->
        match Map.tryFind name functionMap with
        | Some f -> f
        | None -> failwith (sprintf ". %s not implemented" name)
    | _ -> failwith ". expects one symbol"

//

let evalExpressions expressions = 
    eval (doify expressions) Frame.empty

let evalString syntax str = 
    let expressions = parseString syntax str
    evalExpressions expressions

