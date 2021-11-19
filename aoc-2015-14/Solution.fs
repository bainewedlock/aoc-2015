module Solution

open System


let tryParseInt (s:string) =
    match Int32.TryParse s with
    | false, _ -> None
    | true,  x -> Some x

let parse (s:string) =
    let tokens = s.Split ' '
    let numbers = tokens |> Array.choose tryParseInt
    (tokens.[0], numbers.[0], numbers.[1], numbers.[2])

let distanceAfter totalSeconds (_, speed, uptime, downtime) =
    let fullLegs = totalSeconds / (uptime+downtime) |> int
    let lastLegSeconds = totalSeconds % (uptime+downtime)
    let flyingSeconds =
        fullLegs * uptime 
        + Math.Min(lastLegSeconds, uptime) 
    speed * flyingSeconds

let solve (input:string) =
    input.Split "\r\n"
    |> Seq.map (parse >> distanceAfter 2503)
    |> Seq.max

type Reindeer (name:string, speed, uptime, downtime) =
    let mutable _distance = 0
    let mutable _time = 0
    let mutable _score = 0
    member _.name = name
    member _.distance = _distance
    member _.move () =
        if _time < uptime then _distance <- _distance + speed
        _time <- _time + 1
        if _time >= (uptime+downtime) then _time <- 0
    member _.addPoint () = _score <- _score + 1
    member _.score = _score

let doMoveAll (rs:Reindeer List) =
    rs |> List.iter (fun x -> x.move())
    let order = rs |> List.sortByDescending (fun x -> x.distance)
    let topDistance = order.Head.distance
    let leading = rs |> List.filter (fun x -> x.distance = topDistance)
    leading |> List.iter (fun x -> x.addPoint())

let solve2 (input:string) =
    let rs =
        input.Split "\r\n"
        |> Array.toList
        |> List.map (parse >> Reindeer)
    for _ in 1 .. 2503 do
        doMoveAll rs
    rs |> List.map (fun x -> x.score) |> List.max

