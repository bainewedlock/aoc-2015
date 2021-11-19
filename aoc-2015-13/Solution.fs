module Solution


let getFactor = function
    | "gain" -> +1
    | "lose" -> -1

let parse (line:string) =
    let xs = line.Trim('.').Split(' ')
    match xs.[2] with
    | "gain" -> xs.[0], xs.[10], +1 * int xs.[3]
    | "lose" -> xs.[0], xs.[10], -1 * int xs.[3]

let circlePermutations (instructions) =
    let alphabet = Seq.collect (fun (a,b,_) -> [a;b]) instructions
    let rec loop acc (xs:string Set) = [
        if xs.IsEmpty then yield acc else
        for x in xs do yield! loop (x::acc) (xs.Remove x) ]
    let hd = Seq.head alphabet
    loop [hd] ((Set alphabet).Remove hd)
    |> List.map (fun xs -> hd::xs)

let applyRule table (a,b,d) =
    if  table @ [table.Head]
        |> Seq.pairwise
        |> Seq.exists (fun (x,y) -> (a,b)=(x,y) || (b,a)=(x,y))
    then d else 0

let genericSolve extraRules (input:string) =
    let instructions =
        input.Split "\r\n"
        |> Seq.map parse
        |> Seq.append extraRules
    let eval table = Seq.sumBy (applyRule table) instructions
    circlePermutations instructions
    |> List.map eval
    |> List.max

let solve = genericSolve []
let solve2 = genericSolve [("Yourself", "Yourself", 0)]

