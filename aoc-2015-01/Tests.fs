module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "examples" {
                solve "(())" =! 0
                solve "((("  =! 3 } ]
        testList "part 2" [
            test "examples" {
                solve2 ")"     =! 1
                solve2 "()())" =! 5 }
            test "current floor tracking" {
                track [1; 1; 1; -1; 1] |> Seq.toList =! [ 0; 1; 2; 3; 2; 3 ] }
        ]
    ]


