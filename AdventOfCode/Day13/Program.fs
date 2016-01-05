open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let inputlines = File.ReadAllLines("../../input.txt")
    
let makePersonList(): Dictionary<string, List<string*int>> = 
    let result = new Dictionary<string, List<string*int>>()
    let updateList n f v =
        if result.ContainsKey(n) then 
            result.[n].Add (f,v)
        else
            let l = new List<string*int>()
            l.Add(f,v)
            result.Add (n,l)
    let parseLine (l: string) =
        let p = new Regex(@"(\w+)\s.*(gain|lose)\s(\d+).*\s(\w+)\.")
        let m = p.Match(l)
        if (m.Success) then
            let m1 = m.Groups.[1].Value
            let m2 = if m.Groups.[2].Value = "gain" then 1 else -1
            let m3 = int m.Groups.[3].Value * m2
            let m4 = m.Groups.[4].Value
            updateList m1 m4 m3
        else
            failwith ("ERROR Parsing : "+l)
    inputlines
    |> Array.iter parseLine
    result

let list = makePersonList()

// part2 add myself

let persons = list.Keys |> Seq.cast<string> |> Seq.toList
persons |> List.iter (fun p -> list.[p].Add("me", 0))
list.Add("me", new List<string*int>())
persons |> List.iter (fun p -> list.["me"].Add(p, 0))

let personCount = list.Count

let foundHappiness p1 p2 =
    list.[p1] |> Seq.find (fun (p,_) -> p = p2) |> snd

let rec checkSeats person (seatedList: string list) (acc: int) =
    if (seatedList |> List.length = personCount) then 
        // last to first
        let hp = seatedList |> List.head
        let lp = seatedList |> List.last
        // search from headperson to LastPerson happiness
        acc + (foundHappiness hp lp) + (foundHappiness lp hp)
    else 
        let ns = list.[person] |> Seq.filter(fun i -> seatedList |> List.contains (fst i) |> not) |> Seq.toList
        let nacc = [for (p,v) in ns -> checkSeats p (p::seatedList) (acc+v+(foundHappiness p person))]
        nacc |> List.max

let result = 
    list.Keys
    |> Seq.cast<string>
    |> Seq.map (fun p -> checkSeats p [p] 0)
    |> Seq.max

printfn "%d" result

Console.ReadLine() |> ignore
