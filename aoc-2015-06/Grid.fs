
namespace aoc_2015_06

type Grid(width, height) =
    let mutable lights = Array.create (width*height) 0
    member _.Sum () =  Array.sum lights
    member _.Change f (x,y) =
        let i = y*width + x
        lights.[i] <- f lights.[i]
