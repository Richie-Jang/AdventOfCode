// day3
open System
open System.IO
open System.Collections.Generic

let inputfile = "../../input.txt"

let pos = File.ReadAllText(inputfile)

type House = 
    struct
        val mutable X: int
        val mutable Y: int
    end

let mutable curHouse = new House()

let santaPath = new List<House>()

santaPath.Add(curHouse)

for i in pos do
    match i with
    | '^' -> curHouse.Y <- curHouse.Y - 1
    | '>' -> curHouse.X <- curHouse.X + 1
    | '<' -> curHouse.X <- curHouse.X - 1
    | 'v' -> curHouse.Y <- curHouse.Y + 1
    | _ -> ()
    santaPath.Add(curHouse)

let setpaths = santaPath |> Seq.distinct

printfn "All Paths : %d" santaPath.Count
printfn "Result : %d" (setpaths |> Seq.length)

Console.ReadLine() |> ignore