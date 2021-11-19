open Solution

[<EntryPoint>]
let main argv =

    solve "input.txt"
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 1350)"

    solve2 "input.txt"
    |> printfn "solution 2: %A"
    printfn "(accepted answer: 2085)"

    0
