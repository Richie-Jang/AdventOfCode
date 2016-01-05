open System
open System.Text.RegularExpressions

type BitPlus = YES | NO
let incDict = "abcdefghjkmnpqrstuvwxyz" |> Seq.pairwise |> dict
let readStrFromBack (c: char) (bitplus: BitPlus, nstr: char list) =
    match bitplus,c with
    | YES,'z'  -> YES,'a'::nstr
    | YES,n    -> NO ,incDict.[n]::nstr
    | NO, n    -> NO ,n::nstr

let increasingStr (str: string) =
    let newstrlist = Seq.foldBack readStrFromBack str (YES,[]) |> snd
    String.Join("", newstrlist)

let generateSeq = Seq.unfold (fun state -> Some (state, increasingStr state)) >> Seq.skip 1

let (+) (c1:char) (next: int) = char (int c1 + next)
let checkStraightChars (str: string) =
    str |> Seq.windowed 3 |> Seq.exists (fun [|a;b;c|] -> a+1 = b && b+1 = c)

let checkNotContainsIOL (str: string) = 
    str |> Seq.exists (fun i -> ['i';'o';'l'] |> List.contains(i)) |> not

let checkPairCharsAtleast2 (str: string) =
    str |> Seq.windowed 2 |> Seq.filter (fun [|a;b|] -> a = b)
        |> Seq.distinct |> Seq.length >= 2

let checkThreeConditions str =
    checkNotContainsIOL (str) && checkStraightChars (str) && checkPairCharsAtleast2 (str)

let input = "cqjxjnds"

//printfn "%A" (generateSeq input |> Seq.take 100 |> Seq.toList)

let result = 
    generateSeq (input)
    |> Seq.filter checkThreeConditions
    |> Seq.take 2
printfn "%A" (result |> Seq.toList)

//let test =
//    generateSeq input |> Seq.take 100
//
//
//test
//|> Seq.iter (fun i -> printf "%s " i)

printfn "Program Finished..."

//printfn "%A" (checkThreeConditions "cqjxxyzz")

Console.ReadLine() |> ignore