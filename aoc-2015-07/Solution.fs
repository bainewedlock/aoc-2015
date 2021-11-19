module Solution

open System
open System.Collections.Generic

type Input =
    | Number of int
    | NOT of string
    | OR of string * string 
    | AND of string * string 
    | LSHIFT of string * string
    | RSHIFT of string * string
    | Wire of string

let isNumber x = Seq.forall Char.IsDigit x 

let parseLine (l:string) =
    match l.Trim().Split ' ' |> Array.toList with
    | [x;"->";w] when isNumber x            -> w, Number (int x)
    | [x;"->";w]                            -> w, Wire x
    | ["NOT";x;"->";w]                      -> w, NOT x
    | [x;"OR";y;"->";w]                     -> w, OR (x,y)
    | [x;"AND";y;"->";w]                    -> w, AND (x,y)
    | [x;"LSHIFT";y;"->";w] when isNumber y -> w, LSHIFT (x, y)
    | [x;"RSHIFT";y;"->";w] when isNumber y -> w, RSHIFT (x, y)
    | _ -> raise (ArgumentException (sprintf "unexpected line: %s" l))

type State = Dictionary<string, Input>
let toState = Seq.map (fun (a,b) -> KeyValuePair(a, b)) >> State

let rec doEval (s:State) i : int =
    let read w = if isNumber w then int w else doEval s w
    let unary op a    = Number (op (read a))
    let binary op a b = Number (op (read a) (read b))
    match s.[i] with
    | Number _      -> ()
    | Wire a        -> s.[i] <- unary id a
    | NOT a         -> s.[i] <- unary (~~~) a
    | OR (a,b)      -> s.[i] <- binary (|||) a b
    | AND (a,b)     -> s.[i] <- binary (&&&) a b
    | LSHIFT (a, b) -> s.[i] <- binary (<<<) a b
    | RSHIFT (a, b) -> s.[i] <- binary (>>>) a b
    match s.[i] with
    | Number x -> x

let initState (s:string) =
    s.Trim().Split "\r\n"
    |> Seq.toList
    |> List.map parseLine
    |> toState

let solve s =
    let state = initState s 
    doEval state "a"

let solve2 s =
    let state = initState s
    state.["b"] <- Number (solve s)
    doEval state "a"
