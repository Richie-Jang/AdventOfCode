open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let testLines =
    """London to Dublin = 464
       London to Belfast = 518
       Dublin to Belfast = 141"""

let inputpath = "../../input.txt"
let lines = File.ReadAllLines (inputpath)


let parseLine line = 
    let pat = new Regex (@"(\w+)\sto\s(\w+)\s=\s(\d+)")
    let mat = pat.Match (line)
    match mat.Success with
    | true -> (mat.Groups.[1].Value, mat.Groups.[2].Value, int (mat.Groups.[3].Value))
    | _    -> failwith (sprintf "Can not parse : %s" line)

let rec makeDirSet (dirlist: (string*string*int) list) (s: Set<string>) = 
    match dirlist with
    | (a,b,c)::t -> makeDirSet t (s.Add(a).Add(b))
    | []   -> s

// check
let list = 
    //testLines.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    lines
    |> Array.map (fun i -> parseLine (i.Trim()))
    |> Array.toList

//let dirset = makeDirSet list (Set.empty)
// make linkedList
let dirMap = new Dictionary<string, Set<string*int>>()
list
|> List.iter (fun (a,b,c) ->
        if (not <| dirMap.ContainsKey(a)) then dirMap.Add(a, Set.empty.Add(b,c))
        else dirMap.[a] <- dirMap.[a].Add(b,c)
        if (not <| dirMap.ContainsKey(b)) then dirMap.Add(b, Set.empty.Add(a,c))
        else dirMap.[b] <- dirMap.[b].Add(a,c))

let totalCountDirs = dirMap.Keys.Count

let mutable results = []

let rec visitToEnd dir visited dist =
    if (dir = None) || (visited |> Set.count = totalCountDirs) then 
//        if (dir <> None) then printfn "%s -> Total : %d" (dir.Value) dist
//        else printfn "-> Total : %d" dist
//        printfn ""
        results <- dist :: results
    else 
        //printf "%s(%d) -> " (dir.Value) dist
        dirMap.[dir.Value]
        |> Set.iter (fun (d,dd) -> 
            if (not (visited.Contains d)) then 
                visitToEnd (Some d) (visited.Add(d)) (dist+dd))
            
dirMap.Keys |> Seq.iter (fun i ->
    for ii in dirMap.[i] do
        //printf "%s -> " i
        visitToEnd (Some(fst ii)) (Set.empty.Add(i).Add(fst ii)) (snd ii)
)

// get Shortest route
let resultmin = results |> List.min
let resultmax = results |> List.max
printfn "Shortest Route : %d" resultmin
printfn "Longest Route : %d" resultmax

Console.ReadLine() |> ignore