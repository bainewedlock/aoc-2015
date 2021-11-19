module Solution

type Light(c) =
    let mutable _calculated = false
    let mutable _isOn = c = '#'
    let mutable _neighbours = []
    let mutable _stayOn = false
    member _.isOn = _isOn
    member _.calculate () =
        let adjacent = 
            _neighbours
            |> List.filter (fun (l:Light) -> l.isOn)
            |> List.length 
        match _isOn, adjacent with
        | true, x when x < 2 || x > 3 -> _calculated <- false
        | false, 3                    -> _calculated <- true
        | _                           -> _calculated <- _isOn
    member _.tick () = 
        if _stayOn then () else
        _isOn <- _calculated
    member _.debugNeighbours = _neighbours.Length
    member _.setNeighbours xs = _neighbours <- xs |> Seq.toList
    member _.stayOn () =
        _isOn <- true
        _stayOn <- true

let parseLineAt y line =
    line
    |> Seq.toList
    |> List.indexed
    |> List.map (fun (x,c) -> (x,y), Light c)

let parse (input:string) =
    let lights =
        input.Trim().Split "\r\n"
        |> Seq.indexed
        |> Seq.collect (fun (i,line) -> parseLineAt i line)
        |> Map
    for l in lights do
        let x,y = l.Key
        let neighbours =
            List.choose lights.TryFind [
                x-1, y-1
                x+0, y-1
                x+1, y-1
                x-1, y+0
                x+1, y+0
                x-1, y+1
                x+0, y+1
                x+1, y+1 ]
        l.Value.setNeighbours neighbours
    lights

let genericSolve (lights:Light seq) =
    for _ in [1..100] do
        for l in lights do l.calculate ()
        for l in lights do l.tick ()
    lights
    |> Seq.countBy (fun l -> l.isOn)

let solve input =
    let lights = parse input
    genericSolve (lights |> Seq.map (fun x -> x.Value))

let solve2 input =
    let lights = parse input
    (lights.Item (0,0)).stayOn ()
    (lights.Item (0,99)).stayOn ()
    (lights.Item (99,0)).stayOn ()
    (lights.Item (99,99)).stayOn ()
    genericSolve (lights |> Seq.map (fun x -> x.Value))
