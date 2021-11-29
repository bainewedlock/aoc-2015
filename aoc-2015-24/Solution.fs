module Solution

// idea: just search for the smallest container!
// from here: https://www.reddit.com/r/adventofcode/comments/3y1s7f/comment/cy9ufm6/?utm_source=share&utm_medium=web2x&context=3

let rec comb n xs = 
    match n, xs with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let QE = List.map uint64 >> List.reduce (*)

let rec searchFirstContainer itemCount capacity items =
    comb itemCount items
    |> List.filter (fun xs -> List.sum xs = capacity)
    |> function
    | [] -> searchFirstContainer (itemCount+1) capacity items
    | x  -> x

let genericSolve containerCount (input:string) =
    let weights = input.Split "\r\n" |> Array.toList |> List.map int
    let containerWeight = List.sum weights / containerCount
    let minContainerSize =
        weights
        |> List.sortDescending
        |> List.scan (fun sum x -> x+sum) 0
        |> List.takeWhile (fun x -> x < containerWeight)
        |> List.length
    searchFirstContainer minContainerSize containerWeight weights
    |> Seq.map QE
    |> Seq.min
    |> sprintf "%d"

let solve  = genericSolve 3
let solve2 = genericSolve 4


