module Solution

open System

type Rule = string * (string list)

let replace (rs:Rule List) molecule =
    let rec loop fEnd molecule = Set [
        match molecule with
        | []         -> ()
        | atom::rest ->
            for rule_from, rule_to in rs do
                if atom = rule_from
                then yield fEnd (rule_to @ rest)
            yield! loop ((fun xs -> atom::xs) >> fEnd) rest ]
    loop id molecule

let rec splitMolecule (s:string) =
    let rec loop (cs:char list) = [
        match cs with
        | [] -> ()
        | a::b::rest when Char.IsLower b ->
            yield sprintf "%c%c" a b
            yield! loop rest
        | a::rest ->
            yield sprintf "%c" a
            yield! loop rest ]
    loop (s |> Seq.toList)

let parse (line:string) =
    let tokens = line.Split ' '
    tokens.[0], splitMolecule tokens.[2]

let solve (input:string) molecule =
    let rules =
        input.Split "\r\n"
        |> Seq.toList
        |> List.map parse
    replace rules (splitMolecule molecule)
    |> Seq.length

let parse2 (line:string) =
    let tokens = line.Split ' '
    tokens.[2], tokens.[0]

let rnd = Random()

type Rule2 = string * string

let shuffle (xs:Rule2 list) = List.sortBy (fun _ -> rnd.Next()) xs

let reduceOneRule (m:string::rest) (a:string,b:string) =
    match m.IndexOf a with
    | -1 -> None
    | i  -> 
        let m' = m.Substring(0,i) + b + m.Substring(i+a.Length)
        Some (m'::m::rest)

// try random combination of rules, give up after 1000 reductions
let rec reduce (rules:Rule2 list) target molecule =
    let rules = shuffle rules
    let rec loop (m:string list) =
        if m.Length > 1000 then None else
        if m.Head = target then Some m.Tail else
            match rules |> List.tryPick (reduceOneRule m) with
            | None -> None
            | Some x -> loop x
    match loop [molecule] with
    | None   -> reduce rules target molecule
    | Some x -> x.Length

let solve2 (input:string) molecule =
    let rules =
        input.Split "\r\n"
        |> Seq.toList
        |> List.map parse2
    reduce rules "e" molecule

