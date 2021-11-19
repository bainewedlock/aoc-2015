module Solution


let countVowels : string -> int =
    Seq.filter "aeiou".Contains >> Seq.length

let hasDoubleLetter : string -> bool =
    Seq.pairwise >> Seq.exists (fun (a,b) -> a = b)

let hasBadString (s:string) =
    s.Contains "ab" ||
    s.Contains "cd" ||
    s.Contains "pq" ||
    s.Contains "xy"

let isNice (s:string) =
    countVowels s >= 3 &&
    hasDoubleLetter s &&
    hasBadString s = false

let solve (s:string) =
    s.Trim().Split("\r")
    |> Seq.filter isNice
    |> Seq.length

let findPair (s:string) =
    let rec loop offset (s:string) = 
        if s.Length < 4 then None else
        let candidate = s.Substring(0, 2)
        let rest = s.Substring 2
        if rest.Contains candidate then
            let offset2 = offset + s.IndexOf(candidate, 2)
            Some (candidate, offset, offset2)
        else
            loop (offset+1) (s.Substring 1)
    loop 0 s

let rec findRepeat (s:string) =
    if s.Length < 3 then None else 
    if s.[0] = s.[2] then Some s.[0]
    else findRepeat (s.Substring 1)

let isNice2 (s:string) =
    (findPair s).IsSome &&
    (findRepeat s).IsSome

let solve2 (s:string) =
    s.Trim().Split("\r")
    |> Seq.filter isNice2
    |> Seq.length
