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
            test "combinations" {
                let unify = List.map List.sort >> List.sort
                combinations 25 [20;15;10;5;5] |> unify
                =! unify [
                    [20;5]
                    [20;5]
                    [15;10]
                    [15;5;5] ]
            }
        ]
    ]


