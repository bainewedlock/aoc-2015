module Solution

type Distance = string * string * int

let parse (l:string) =
    let xs = l.Split " "
    xs.[0], xs.[2], int xs.[4]

let calcLegs (xs:Distance list) =
    xs
    |> List.collect (fun (a,b,d) -> [ (a,b), d; (b,a), d ])
    |> Map

let locations (xs:Distance list) =
    xs
    |> List.collect (fun (a,b,_) -> [ a; b ])
    |> Set

let possibleRoutes (xs:Distance list) =
    let rec loop (ls:string Set) (path:string list) = [
        if ls.IsEmpty then yield path else
        for l in ls do
            let ls' = ls.Remove l
            let path' = l::path
            yield! loop ls' path' ]
    loop (locations xs) []

let parseInput (input:string) =
    input.Split "\r\n"
    |> Array.toList
    |> List.map parse

let genericSolve (input:string) =
    let distances = parseInput input
    let legs      = calcLegs distances
    let dist pair = legs.Item pair
    possibleRoutes distances
    |> Seq.map (Seq.pairwise >> Seq.sumBy dist)

let solve = genericSolve >> Seq.min
let solve2 = genericSolve >> Seq.max