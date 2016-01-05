open System
open System.IO
open System.Text.RegularExpressions

//let comet = (14,10,127)
//let dancer = (16,11,162)

let input = File.ReadAllLines ("../../input.txt")

let (=~) p l = 
    let pat = new Regex(p) 
    pat.Match(l)

let reindeers = 
    let parseLine s =
        let m = "^(\w+).*\s(\d+)\s.*\s(\d+)\s.*\s(\d+)\s.*\.$" =~ s
        if m.Success then
            (int m.Groups.[2].Value, int m.Groups.[3].Value, int m.Groups.[4].Value)
        else
            failwith ("Error : "+s)
    input |> Array.map parseLine

let speed t = match t with | (a,_,_) -> a
let duration t = match t with | (_,a,_) -> a
let rest t = match t with | (_,_,a) -> a

let generateSeq (reindeer: int*int*int) =
    let func (index, dist, restcount) =
        match index < duration reindeer with
        | true -> 
            let totaldist = dist + (speed reindeer)
            Some (totaldist, (index+1, totaldist, restcount))
        | false ->
            let r = restcount+1
            if (r <= rest reindeer) then Some(dist, (index+1, dist, r))
            else
                let r = 0
                let totaldist = dist + (speed reindeer)
                Some (totaldist, (1,totaldist,r))
    Seq.unfold func (0,0,0)

// Part1

let getLastRace count reindeer = 
    generateSeq reindeer |> Seq.take count |> Seq.last

let foundMaxDistance count =
    reindeers
    |> Array.map (getLastRace count) |> Array.max

printfn "foundMax Distance (%d) : %d" 2503 (foundMaxDistance 2503)

// Part2

let makeRaces count = 
    reindeers
    |> Array.map (generateSeq >> Seq.take(count) >> Seq.toArray)

let races = makeRaces 2503

let reinCount = reindeers.Length

let rTable = Array.init(reinCount) (fun _ -> Array.create 2503 0)

for i = 0 to 2503-1 do
    let maxdist = [for j in 0 .. (reinCount-1) -> races.[j].[i]] |> List.max
    for j in 0 .. (reinCount-1) do
        let cur = races.[j].[i]
        if cur = maxdist then rTable.[j].[i] <- 1

let datas = [for i = 0 to reinCount-1 do yield (rTable.[i] |> Array.sum)]
printfn "Max Points : %d" (datas |> List.max)

// SOVLED !!!

//printfn "%d" (generateSeq comet |> Seq.take(1000) |> Seq.last)
//printfn "%d" (generateSeq dancer |> Seq.take(1000) |> Seq.last)
//
//// check Points
//
//let arrComet = generateSeq comet |> Seq.take(1000) |> Seq.toArray
//let arrDancer = generateSeq dancer |> Seq.take(1000) |> Seq.toArray
//
//let results = Array.init 2 (fun _ -> Array.create 1000 0)
//
//for i in 0 .. (1000-1) do
//    let c = arrComet.[i]
//    let d = arrDancer.[i]
//    if c > d then results.[0].[i] <- 1
//    elif c < d then results.[1].[i] <- 1
//    else 
//        results.[0].[i] <- 1
//        results.[1].[i] <- 1
//
//let sums = [for i in 0 .. (2-1) -> results.[i] |> Array.sum]
//printfn "%A" sums
//
//// corrected

Console.ReadLine() |> ignore