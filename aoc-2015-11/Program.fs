open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %A"
    printfn "(accepted answer: vzbxxyzz)"

    solve (solve input)
    |> printfn "solution 2: %A"
    printfn "(accepted answer: vzcaabcc)"

    0
