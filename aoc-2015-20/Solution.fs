module Solution


type Result = Lower | Higher | Found

let presents n factor limit =
    let xs = Array.zeroCreate n
    for elf in 1..n do
        let capped = min n (elf+limit*elf-1)
        for i in elf..elf..capped do
            xs.[i-1] <- xs.[i-1] + elf * factor
    xs

let genericSolve factor presentLimit (input:string) =
    let limit = int input
    let ps = presents (limit / 10) factor presentLimit
    ps |> Array.findIndex ((<=)limit) |> (+)1

let solve = genericSolve 10 9999999

let solve2 = genericSolve 11 50
