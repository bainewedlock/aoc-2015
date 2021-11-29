open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 10723906903)"

    solve2 input
    |> printfn "solution 2: %A"
    printfn "(accepted answer: ?)"
    printfn "83754553 is too high"

    0
