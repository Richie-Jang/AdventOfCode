open System
open System.IO
open System.Net
open System.Collections.Generic

type Paper = {l:int; w:int; h:int}

let inputfile = "../../input.txt"

let papers = new List<Paper>()

let parse (l: string) =
    let v = l.Split ('x') |> Seq.map (fun i -> (int i)) |> Seq.sort
    {l = (v |> Seq.item 0); w = (v |> Seq.item 1); h = (v |> Seq.item 2)}

File.ReadLines(inputfile)
|> Seq.iter (fun i -> 
                let p = (parse i)
                papers.Add(p))

Console.WriteLine ("Loading Finished : {0}", papers.Count) 

let getLengthPaper (p: Paper) =
    2 * (p.l * p.w) + 2 * (p.w * p.h) + 2 * (p.h * p.l) + (p.l * p.w)

let sum = 
    papers 
    |> Seq.map getLengthPaper
    |> Seq.sum

printfn "Sum : %d" sum

Console.ReadLine() |> ignore