module Solution

let combinations volume containers =
    let rec loop acc v cs = [
        match v, cs with
        | 0, _  -> yield acc
        | _, [] -> ()
        | _     ->
            for i in [1..cs.Length] do
                let c = cs.[i-1]
                let rest = cs |> List.skip i
                yield! loop (c::acc) (v-c) (rest) ]
    loop [] volume containers

let genericSolve (input:string) =
    input.Split "\r\n"
    |> Array.toList
    |> List.map int
    |> combinations 150

let solve = genericSolve >> List.length

let solve2 (input:string) =
    let xs = 
        genericSolve input
        |> List.sortBy List.length
    xs
    |> Seq.takeWhile (List.length >> (=)xs.Head.Length)
    |> Seq.length
