module Solution


let combinations n (xs:int List) = 
    let rec loop acc n (rest:int List) = seq [
        if n = 0 then yield acc else
        for i in 0..rest.Length-n do
            let x = rest.[i]
            let rest' = rest |> List.skip (i+1)
            yield! loop (x::acc) (n-1) rest' ]
    loop [] n (List.sortDescending xs)

let rec search containerCount acc xs =
    let xs = List.sortDescending xs
    if containerCount = 1 then xs::acc else
    let containerSum = (Seq.sum xs) / containerCount
    let rec searchFor itemsPerContainer =
        if itemsPerContainer > Seq.length xs then [] else
        combinations itemsPerContainer xs
        //|> Seq.takeWhile (Seq.sum >> (<=)containerSum) // pruning
        |> Seq.filter    (Seq.sum >> (=)containerSum)
        |> Seq.filter (fun c ->
            let rest = List.except c xs 
            let subSolutions = search (containerCount-1) (c::acc) rest
            not subSolutions.IsEmpty)
        |> Seq.toList
        |> function
            | [] -> searchFor (itemsPerContainer+1)
            | xs -> xs
    searchFor 1

let QE : int list -> uint64  = Seq.map uint64 >> Seq.reduce (*)

let solve (input:string) =
    let numbers = input.Split "\r\n" |> Array.toList |> List.map int
    search 3 [] numbers
    |> List.minBy (Seq.length)
    |> QE
    |> sprintf "%d"

let solve2 (input:string) =
    let numbers = input.Split "\r\n" |> Array.toList |> List.map int
    search 4 [] numbers
    //|> Seq.minBy (fun xs -> Seq.length xs, QE xs)
    //|> QE
    //|> sprintf "%d"

