module Solution

open System.Text.RegularExpressions

let diagonal n = seq [ for x in 1..n do yield n+1-x, x ]

let order () =
    Seq.initInfinite ((+)1)
    |> Seq.collect diagonal

let next =
    uint64
    >> (*)252533UL
    >> (fun x -> x % 33554393UL)
    >> int

let solvePos pos =
    order ()
    |> Seq.takeWhile ((<>)pos)
    |> Seq.fold (fun acc _ -> next acc) 20151125

let parse input =
    let m = Regex.Match(input, "row (\d+), column (\d+)")
    int m.Groups.[1].Value, int m.Groups.[2].Value

let solve = parse >> solvePos
