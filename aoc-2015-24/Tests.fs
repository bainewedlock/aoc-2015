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
            test "combinations of 1 element" {
                combinations 1 [1;2;3] |> Seq.toList =! [
                    [3]
                    [2]
                    [1] ] }
            test "combinations of 2 elements" {
                combinations 2 [1;2;3] |> Seq.toList =! [
                    [2;3]
                    [1;3]
                    [1;2] ] }
            test "search for 1 container" {
                search 1 [] [1;2;3] =! [[3;2;1]] }
            test "search for 2 containers" {
                search 2 [] [1;2;3;4] =! [[1;4];[2;3]] }
            test "demoinput" { solve demoinput =! "99" }
        ]
        testList "part 2" [
            //test "demoinput" { solve2 demoinput =! "44" }
        ]
    ]


