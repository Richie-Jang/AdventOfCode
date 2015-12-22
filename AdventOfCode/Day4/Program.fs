open System
open System.Security.Cryptography
open System.Text

let security = "yzbqklnj"
let md5 = MD5.Create()

let generateHash (md5: MD5) (key: string) =
    let data = md5.ComputeHash(Encoding.UTF8.GetBytes(key))
    let sbuilder = new StringBuilder()
    for i=0 to data.Length-1 do
        sbuilder.Append(data.[i].ToString("x2")) |> ignore
    sbuilder.ToString()

let loopint = seq {1 .. Int32.MaxValue}

let find = 
    loopint
    |> Seq.map (fun i -> generateHash md5 (security + i.ToString()), i)
    |> Seq.find (fun i -> (fst i).StartsWith("00000"))

printfn "%d" (snd find)

Console.ReadLine() |> ignore