open System
open System.Text.RegularExpressions
open System.IO

let input = File.ReadAllText("../../input.txt")

let getNumbersOnly input =
    let pat = new Regex(@"([\-\d]+)")
    let ms = pat.Matches(input)
    ms
    |> Seq.cast<Match>
    |> Seq.map (fun m -> int m.Value)
    |> Seq.toList
    

//let results = getNumbersOnly input
//let result = results |> List.sum//printfn "%d" result

// part2

// refer solution from "theburningmonk.com/2015/12/advent-of-code-f-day-12/"

let findBoundary sidx direction o c = 
    let rec loop idx direction acc =
        if acc = 0 then idx
        else
            let idx = idx + direction
            let ch = input.[idx]
            if ch = o then loop idx direction (acc+1)
            elif ch = c then loop idx direction (acc-1)
            else loop idx direction acc
    loop sidx direction 1

let findBoundaryL idx = findBoundary idx -1 '}' '{'
let findBoundaryR idx = findBoundary idx 1 '{' '}'

//let bounds =
//    Regex.Matches (input, ":\"red\"")
//    |> Seq.cast<Match>
//    |> Seq.map (fun m ->
//        let s = findBoundaryL m.Index
//        let e = findBoundaryR m.Index
//        s,e)
//    |> Seq.toArray
//
//printfn "%A" bounds

// using Newton JSon Library for Studying
// refer to "https://www.youtube.com/watch?v=0JScrBwF3TA"
// JSon library is first time using for me.

open Newtonsoft.Json.Linq
let data = JObject.Parse(input)

let isRed (token: JObject) = 
    token.Properties()
    |> Seq.exists (fun p -> 
        match p.Value with
        | :? JValue as t when (string t) = "red" -> true
        | _ -> false
    )
     
let shouldAvoid avoid (jo: JObject) =
    jo.Properties()
    |> Seq.exists (fun p -> 
        match p.Value with
        | :? JValue as t -> t.Value = avoid
        | _ -> false)

let rec getSum avoid (token: JToken) = 
    match token with
    | :? JObject as t ->
        if shouldAvoid avoid t then 0L
        else 
            t.Properties()
            |> Seq.map (fun p -> p.Value) 
            |> Seq.map (getSum avoid) 
            |> Seq.sum
    | :? JArray as t ->
        t |> Seq.cast<JToken> |> Seq.map (getSum avoid) |> Seq.sum
    | :? JValue as t ->
        if t.Type = JTokenType.Integer then t.Value :?> int64 else 0L
    | _ -> failwith (sprintf "unknown %A" (token.GetType()))
    
let result = getSum "red" data
printfn "%d" result

let getValueFromProperty (t: JProperty) = t.Value
let checkRedIn (t: JObject): bool =
    t.Properties()
    |> Seq.map getValueFromProperty
    |> Seq.exists (fun i -> i.Type = JTokenType.String && (string i) = "red")

let rec getSum2 (token: JToken): int64 =
    match token with
    | :? JObject as o ->
        if checkRedIn o then 0L
        else
            o.Properties()
            |> Seq.map (getValueFromProperty >> getSum2)
            |> Seq.sum
    | :? JValue  as o -> 
        if (o.Type = JTokenType.Integer) then (o.Value :?> int64) else 0L
    | :? JArray as o  ->
        o |> Seq.cast<JToken> |> Seq.map getSum2 |> Seq.sum
           
    | _  -> failwith (sprintf "Error from %A" token.Type)
   
printfn "%d" (getSum2 data)

Console.ReadLine() |> ignore