module Solution

type Command =
    | HLF of char
    | TPL of char
    | INC of char
    | JMP of int
    | JIE of char * int
    | JIO of char * int

type Computer = { a: int; b: int; pc: int }

let parse (line:string) =
    match line.Split ' ' |> Array.toList with
    | ["hlf";"a"] -> HLF 'a'
    | ["tpl";"a"] -> TPL 'a'
    | ["inc";"a"] -> INC 'a'
    | ["inc";"b"] -> INC 'b'
    | ["jmp";x]   -> JMP (int x)
    | ["jie";x;y] -> JIE (x.[0], int y)
    | ["jio";x;y] -> JIO (x.[0], int y)
    | _           -> failwithf "unexpected line: %s" line

let exec cmd c =
    match cmd with
    | HLF 'a' -> { c with pc = c.pc + 1; a = c.a / 2 }
    | TPL 'a' -> { c with pc = c.pc + 1; a = c.a * 3 }
    | INC 'a' -> { c with pc = c.pc + 1; a = c.a + 1 }
    | INC 'b' -> { c with pc = c.pc + 1; b = c.b + 1 }
    | JMP x   -> { c with pc = c.pc + x }
    | JIE ('a', x) when c.a % 2 = 0 -> { c with pc = c.pc + x }
    | JIE ('a', _)                  -> { c with pc = c.pc + 1 }
    | JIO ('a', x) when c.a = 1     -> { c with pc = c.pc + x }
    | JIO ('a', _)                  -> { c with pc = c.pc + 1 }
    | _ -> failwithf "unexpected command: %A" cmd

let parseInput (input:string) =
    input.Trim().Split "\r\n"
    |> Array.toList
    |> List.map parse
    |> List.indexed
    |> Map

let rec runToEnd prog computer =
    match Map.tryFind computer.pc prog with
    | None     -> computer
    | Some cmd -> computer |> exec cmd |> runToEnd prog

let genericSolve a (input:string) =
    let result = runToEnd (parseInput input) { a=a; b=0; pc=0 }
    result.b

let solve = genericSolve 0
let solve2 = genericSolve 1
