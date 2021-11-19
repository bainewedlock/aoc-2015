module Solution

let rec rule1 xs =
    match xs with
    | [] -> false
    | a::b::c::_ when a+1 = b && b+1 = c -> true
    | _::rest -> rule1 rest

let bad = Set ['i';'o';'l']

let rec rule2 = List.exists bad.Contains >> not

let rule3 xs =
    xs
    |> List.pairwise
    |> List.filter (fun (a,b) -> a=b)
    |> List.map fst
    |> List.distinct
    |> List.length
    |> (<)1

let rec inc xs =
    let rec loop =
        function
        | [] -> []
        | 'z'::rest -> 'a'::loop rest
        | c::rest   -> ((int c) + 1 |> char)::rest
    xs
    |> List.rev
    |> loop
    |> List.rev

let check (xs:char list) =
    rule1 (xs |> List.map int)
        && rule2 xs
        && rule3 xs

let solve (s:string) =
    s
    |> Seq.toList
    |> Seq.unfold(fun x -> Some (inc x, inc x))
    |> Seq.find check
    |> List.map string
    |> String.concat ""
    


