module Runtime

open Expression

let essentialFunctions = [
    { Name = "list"; F = List }
    { Name = "head"; F = fun (e::_) -> e }
    { Name = "tail"; F = fun (_::t) -> List t }
    ]

let functionMap = 
    let pairs = List.map (fun (f:Function) -> f.Name, f.F) essentialFunctions
    Map.ofList pairs
