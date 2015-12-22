open System
open System.IO

let inputfile = "../../input.txt"

let lines = File.ReadLines(inputfile)

let checkLine1 line =
    // aeiou
    let mutable result = false
    let case1 = 
        line 
        |> Seq.filter
            (fun i -> i = 'a' || i = 'e' || i = 'i' || i = 'o' || i = 'u')
        |> Seq.countBy (fun i -> i)
    //let case1count = case1 |> Seq.length
    if (case1 |> Seq.length) >= 3 then result <- true
    else
        let f = case1 |> Seq.tryFind(fun i -> (snd i) >= 3)
        if f.IsSome then result <- true
    result

let checkLine2 (line: string) =
    // twice in a row
    let counter = ref 0
    let doubleCount = 
        line
        |> Seq.take(line.Length-1)
        |> Seq.filter
            (fun i -> 
                let c1 = i
                let c2 = line.[!counter+1]
                incr counter
                c1 = c2
            )
        |> Seq.length
    if doubleCount > 0 then true else false

    

let checkNiceLine3 (niceline: string) =
    // not contain ab cd pq xy
    match niceline with
    | a when a.Contains ("ab") -> false
    | a when a.Contains ("cd") -> false
    | a when a.Contains ("pq") -> false
    | a when a.Contains ("xy") -> false
    | _ -> true

//// test Code
//let checkString str =
//    match (checkNiceLine3 str) with
//    | true -> 
//        (checkLine1 str) && (checkLine2 str)
//    | false -> false


// check lines

let findNice = 
    lines
    |> Seq.filter(fun i -> checkNiceLine3 i)
    |> Seq.filter(fun i -> (checkLine1 i) && (checkLine2 i))
    |> Seq.length

printfn "%d" findNice

Console.ReadLine() |> ignore