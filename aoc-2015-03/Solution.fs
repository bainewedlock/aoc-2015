module Solution

let move (x,y)  = function
    | '>' -> x+1,y+0
    | '<' -> x-1,y+0
    | '^' -> x+0,y+1
    | 'v' -> x+0,y-1

let rec track history steps =
    match steps, history with
    | [], _           -> history
    | c::rest, pos::_ -> let pos2 = move pos c
                         track (pos2::history) rest

let solve (input:string) =
    track [0,0] (input.ToCharArray() |> Array.toList)
    |> Set.ofSeq
    |> Set.count

let even x = x % 2 = 0

let partitionIndexed f =
    List.indexed
    >> List.partition f
    >> fun (a,b) -> [a;b]
    >> List.map (List.map snd)

let solve2 (input:string) =
    input.ToCharArray()
    |> Array.toList
    |> partitionIndexed (fst >> even)
    |> List.map (track [0,0])
    |> List.concat
    |> Set.ofList
    |> Set.count

