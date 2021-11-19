module Solution

open System
open System.Text
open System.Security.Cryptography


let md5 (s:string) =
    MD5.Create().ComputeHash(Encoding.UTF8.GetBytes s)
    |> Seq.map (fun b -> b.ToString "x2")
    |> String.concat ""
    
let genericSolve2 n x =
    let target = String.replicate n "0"
    let rec loop i =
        let candidate = sprintf "%s%d" x i
        if (md5 candidate).StartsWith target then
            i
        else
            loop (i+1)
    loop 1

let timer f =
    let s = DateTime.UtcNow
    let result = f ()
    let elapsed = DateTime.UtcNow - s
    printfn "Elapsed: %A" elapsed
    result

let genericSolve part n x =
    printfn "solving %s..." part
    timer (fun () -> genericSolve2 n x)

let solve =  genericSolve "part 1" 5

let solve2 = genericSolve "part 2" 6
