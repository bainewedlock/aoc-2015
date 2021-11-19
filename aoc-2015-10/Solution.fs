module Solution

let compress xs =
    let rec loop acc = function
        | [] -> acc
        | (a,x)::(b,y)::xs when x=y -> loop acc ((a+b,x)::xs)
        | x::rest -> loop (x::acc) rest
    loop [] xs
    |> List.rev

let toCounts (s:string) =
    s |> Seq.map (fun c -> (1, (int c) - (int '0'))) |> Seq.toList

let rec nextItem i xs =
    if i = 0 then xs else
    xs
    |> compress
    |> List.collect (fun (a,x) -> [1,a; 1,x])
    |> nextItem (i-1)

let genericSolve n = toCounts >> nextItem n >> List.length

let solve = genericSolve 40
let solve2 = genericSolve 50
