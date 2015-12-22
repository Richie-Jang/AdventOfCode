open System
open System.IO

let inputfile = "../../input.txt"

let lines = File.ReadLines(inputfile) |> Seq.filter (fun i -> i <> "")

let checkLine1 line =
    // aeiou
    let mutable result = false
    let case1 = 
        line 
        |> Seq.filter
            (fun i -> i = 'a' || i = 'e' || i = 'i' || i = 'o' || i = 'u')
        |> Seq.toList
    //let distinctcase1 = case1 |> Seq.distinct |> Seq.toList
    //let test = sprintf "%A" distinctcase1
    if (case1 |> Seq.length) >= 3 then result <- true
    result

let checkLine2 (line: string) =
    // twice in a row
    let result = ref false
    for i = 0 to line.Length-2 do
        let c1 = line.[i]
        let c2 = line.[i+1]
        if c1 = c2 then result := true
    !result
   
let checkNiceLine3 (niceline: string) =
    // not contain ab cd pq xy
    match niceline with
    | a when a.Contains ("ab") -> false
    | a when a.Contains ("cd") -> false
    | a when a.Contains ("pq") -> false
    | a when a.Contains ("xy") -> false
    | _ -> true

// test Code
let checkString str =
    match (checkNiceLine3 str) with
    | true -> 
        (checkLine1 str) && (checkLine2 str)
    | false -> false


// check lines
open System.Text.RegularExpressions

// three Vowels
let checkFirst line =
    let ms = Regex.Matches (line, ".*([aeiou].*){3,}.*")
//    if (ms.Count > 0) then
//        for i = 0 to ms.Count-1 do
//            let mmm = ms.Item i
//            for j = 0 to mmm.Groups.Count-1 do
//                let mmmm = mmm.Groups.Item j
//                printfn "[%d] %s" j mmmm.Value
    ms.Count > 0

// hasDoubleLetter
let checkSecond line =
    let ms = Regex.Matches (line, ".*(.)\\1.*")
    ms.Count > 0

let checkThird line =
    let ms = Regex.Matches (line, ".*(ab|cd|pq|xy)+.*")
    ms.Count = 0

let findNice1 =
    lines
    |> Seq.filter(fun i -> (checkLine1 i) && (checkLine2 i) && (checkNiceLine3 i))
    |> Seq.toList

let findNice = 
    lines
    //|> Seq.filter(fun i -> (checkLine1 i) && (checkLine2 i) && (checkNiceLine3 i))
    |> Seq.filter (fun i -> checkFirst i && checkSecond i && checkThird i)
    |> Seq.toList

File.WriteAllLines ("c:\\Temp\\test1.txt", findNice1)
File.WriteAllLines ("c:\\Temp\\test2.txt", findNice)

printfn "MyWay -> %d" (findNice1.Length)
printfn "RegWay -> %d" findNice.Length

Console.ReadLine() |> ignore