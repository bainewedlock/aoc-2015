module Solution

open aoc_2015_06

type Rect = int * int * int * int

type Action = TurnOn | TurnOff | Toggle

let parse (l:string) =
    let tokens = l.Replace("turn ", "").Split [|' ';','|]
    let x1 = int tokens.[1]
    let y1 = int tokens.[2]
    let x2 = int tokens.[4]
    let y2 = int tokens.[5]
    match tokens.[0] with
    | "off"    -> TurnOff, (x1, y1, x2, y2)
    | "on"     -> TurnOn,  (x1, y1, x2, y2)
    | "toggle" -> Toggle,  (x1, y1, x2, y2)
    | _        -> failwithf "unexpected line: %s" l

let doInRect (grid:Grid) (x1, y1, x2, y2) f =
    for y in y1..y2 do
    for x in x1..x2 do
        grid.Change f (x,y)

let part1actions = function
    | Toggle  -> fun x -> 1-x
    | TurnOn  -> fun _ -> 1
    | TurnOff -> fun _ -> 0

let doExecute actionMap (grid:Grid) (action, (x1,y1,x2,y2)) =
    let f = actionMap action
    for y in y1..y2 do
    for x in x1..x2 do
        grid.Change f (x,y)

let solveGeneric actions (s:string) =
    let g = Grid(1000, 1000)
    s.Trim().Split "\n"
    |> Array.map parse
    |> Array.iter (doExecute actions g)
    g.Sum()

let solve = solveGeneric part1actions

let part2actions = function
    | Toggle  -> fun x -> x+2
    | TurnOn  -> fun x -> x+1
    | TurnOff -> fun x -> max 0 (x-1)

let solve2 = solveGeneric part2actions
