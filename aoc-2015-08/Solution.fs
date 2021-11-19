module Solution

open System.Text.RegularExpressions
open System.IO


let replaceAll (x:string) (y:string) (s:string) =
    Regex.Replace(s, x, y)

let eval (s:string) =
    s.Substring(1, s.Length-2)
    |> replaceAll @"\\\\" "#"
    |> replaceAll @"\\x.." "#"
    |> replaceAll @"\\." "#"
    |> Seq.length

let count (s:string) = s.Length

let solve (path:string) =
    let xs = File.ReadAllLines path
    let a = xs |> Seq.sumBy count
    let b = xs |> Seq.sumBy eval
    a-b

let encode (s:string) =
    s
    |> replaceAll @"\\" "##"
    |> replaceAll "\"" "##"
    |> Seq.length
    |> (+) 2

let solve2 (path:string) =
    let xs = File.ReadAllLines path
    let a = xs |> Seq.sumBy encode
    let b = xs |> Seq.sumBy count
    a - b
