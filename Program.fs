//GREEN
//5.1
let sum a b =
    let rec sumAcc a b acc =
        match b with
        | 0 -> (acc + a)
        | _ -> sumAcc a (b - 1) (acc + (a + b))
    sumAcc a b 0

//5.2
let length lst =
    let rec lengthAcc lst' acc =
        match lst' with
        | [] -> acc
        | x :: xs -> lengthAcc xs (acc + 1)
    lengthAcc lst 0

//5.3
let foldBack (f: ('a -> 'b -> 'b)) (lst: 'a list) (acc:'b) : 'b =
    let rec foldBackC (lst': 'a list)  c : 'b =
        match lst' with
        | [] -> c acc
        | x :: xs -> foldBackC xs (fun acc' -> c (f x acc'))
    foldBackC lst id

//5.4
let factC x =
    let rec aux x' c =
        match x' with
        | 0 -> c 1
        | x' -> aux (x' - 1) (fun acc -> c (acc * x'))
    aux x id

//YEELOW
//5.5
let fibA n =
    let rec aux n' acc acc2=
        match n' with
        | n' when n' > n -> acc 
        | _ -> aux (n' + 1) acc2 (acc + acc2)
    aux 1 0 1

let fibC n =
    let rec aux n' c =
        match n' with
        | 0 -> c 0
        | 1 -> c 1
        | _ -> aux (n' - 2) (fun x -> aux (n' - 1) (fun y -> c (x + y)))
    aux n id

//5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)
//Why not iterative??
(*
    It is not iterative because 1 is added to c res every run. 1 should be added inside c => run res -> c (1::res), 
    else the continuation will not work at all
*)

let rec bigListKFixed c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> c (1:: res)) (n - 1)


//RED
//5.7



//TESTING
let test expected actual =
    if expected = actual then
        "Passed"
    else
        sprintf "Failed: Expected: %A Actual: %A" expected actual


[<EntryPoint>]
let main argv =
    printfn "GREEN"
    printfn "5.1 sum: %s" (test 6 (sum 0 3))
    printfn "5.2 length: %s" (test 3 (length [1;2;3]))
    printfn "5.3 foldBack: %s" (test 6 (foldBack (fun a b -> a + b) [1;2;3] 0))
    printfn "5.4 fact: %s" (test 24 (factC 4))
    printfn ""
    printfn "YELLOW"
    printfn "5.5 fibA: %s" (test 0 (fibA 0))
    printfn "5.5 fibA: %s" (test 1 (fibA 1))
    printfn "5.5 fibA: %s" (test 1 (fibA 2))
    printfn "5.5 fibA: %s" (test 2 (fibA 3))
    printfn "5.5 fibA: %s" (test 3 (fibA 4))
    printfn "5.5 fibA: %s" (test 5 (fibA 5))
    printfn "5.5 fibA: %s" (test 8 (fibA 6))
    printfn "5.5 fibC: %s" (test 0 (fibC 0))
    printfn "5.5 fibC: %s" (test 1 (fibC 1))
    printfn "5.5 fibC: %s" (test 1 (fibC 2))
    printfn "5.5 fibC: %s" (test 2 (fibC 3))
    printfn "5.5 fibC: %s" (test 3 (fibC 4))
    printfn "5.5 fibC: %s" (test 5 (fibC 5))
    printfn "5.5 fibC: %s" (test 8 (fibC 6))
    printfn "5.6 bigListKFixed: %s" (test 130000 (length (bigListKFixed id 130000)))
    0