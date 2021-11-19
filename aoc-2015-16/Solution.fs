module Solution

let parse (line:string) =
    let tokens = line.Replace(":", "").Replace(",", "").Split ' '
    let auntNo = int tokens.[1]
    let properties =
        tokens
        |> Array.toList
        |> List.skip 2
        |> List.chunkBySize 2
        |> List.map (fun [k;v] -> k, int v)
    int auntNo, properties

let rules_part1 = function
    | "children"    -> (=)3
    | "cats"        -> (=)7
    | "samoyeds"    -> (=)2
    | "pomeranians" -> (=)3
    | "akitas"      -> (=)0
    | "vizslas"     -> (=)0
    | "goldfish"    -> (=)5
    | "trees"       -> (=)3
    | "cars"        -> (=)2
    | "perfumes"    -> (=)1

let rules_part2 = function
    | "children"    -> (=)3
    | "cats"        -> (<)7
    | "samoyeds"    -> (=)2
    | "pomeranians" -> (>)3
    | "akitas"      -> (=)0
    | "vizslas"     -> (=)0
    | "goldfish"    -> (>)5
    | "trees"       -> (<)3
    | "cars"        -> (=)2
    | "perfumes"    -> (=)1

let diff rules =
    List.filter (fun (k,v) -> rules k v |> not)
    >> List.map fst

let genericSolve rules (input:string) =
    input.Split "\r\n"
    |> Array.toList
    |> List.map parse
    |> List.filter (snd >> diff rules >> List.isEmpty)

let solve  = genericSolve rules_part1

let solve2 = genericSolve rules_part2
