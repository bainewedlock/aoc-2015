open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 1269)"
    printfn "solution: https://gist.github.com/latkin/45d8e009f471b4b5d609"

    solve2 input
    |> printfn "solution 2: %A"
    printfn "(accepted answer: 1309)"

    0
