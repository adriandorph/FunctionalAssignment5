
//TESTING
let test expected actual =
    if expected = actual then
        "Passed"
    else
        sprintf "Failed: Expected: %A Actual: %A" expected actual


[<EntryPoint>]
let main argv =
    
    0