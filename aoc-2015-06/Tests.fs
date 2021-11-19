module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open aoc_2015_06
open Solution

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "parsing" {
                parse "turn on 1,20 through 300,999" =!
                    (TurnOn, (1, 20, 300, 999))
                parse "turn off 0,0 through 0,0" =!
                    (TurnOff, (0, 0, 0, 0))
                parse "toggle 0,0 through 0,0" =!
                    (Toggle, (0, 0, 0, 0)) }
            test "execute commands" {
                let g = Grid(2, 2)
                doExecute part1actions g (Toggle, (0, 0, 0, 0))
                g.Sum() =! 1
                doExecute part1actions g (Toggle, (0, 0, 1, 1))
                g.Sum() =! 3 }
            test "examples" {
                let g = Grid(1000, 1000)
                doExecute part1actions g (TurnOn, (0,0, 999,999))
                g.Sum() =! 1000000

                let g = Grid(1000, 1000)
                doExecute part1actions g (Toggle, (0,0, 999,0))
                g.Sum() =! 1000

                let g = Grid(1000, 1000)
                doExecute part1actions g (Toggle, (499,499, 500,500))
                g.Sum() =! 4 } ]
        testList "part 2" [
            test "examples" {
                let g = Grid(1000, 1000)
                doExecute part2actions g (Toggle, (0,0, 999,999))
                g.Sum() =! 2000000
            }
            test "dont go below zero" {
                let g = Grid(1, 1)
                doExecute part2actions g (TurnOff, (0,0, 0,0))
                g.Sum() =! 0 } ]
    ]


