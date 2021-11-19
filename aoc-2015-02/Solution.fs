module Solution

let wrap (l,w,h) =
    let surfaces = [ l*w ; w*h ; h*l ]
    2 * (List.sum surfaces) + (List.min surfaces)

let parse (s:string) =
    let [|l;w;h|] = s.Split('x') |> Array.map int
    l,w,h

let ribbon (l,w,h) =
    let a::b::c::_ = [l;w;h] |> List.sort 
    a+a+b+b + a*b*c

let genericSolve f (s:string) =
    s.Trim().Split '\n'
    |> Array.map (parse >> f)
    |> Array.sum

let solve = genericSolve wrap

let solve2 = genericSolve ribbon
