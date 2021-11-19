module Solution

let parseChar = function
    | '(' ->  1
    | ')' -> -1
    | _   ->  0

let parseString (s:string) = Seq.map parseChar s

let solve = parseString >> Seq.sum

let track = Seq.scan (+) 0

let solve2 =
    parseString
    >> track
    >> Seq.findIndex ((=) -1)
