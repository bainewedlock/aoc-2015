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
            test "parsing" {
                parse ("Alice would gain 54 happiness units by "
                    + "sitting next to Bob.") =! ("Alice", "Bob", +54)
                parse ("David would lose 7 happiness units by " 
                    + "sitting next to Bob.") =! ("David", "Bob", -7) }
            test "permutations" {
                circlePermutations [
                    ("Alice", "Bob", 0)
                    ("Alice", "Charlie", 0)
                ] |> Set =! Set [
                    ["Alice"; "Bob"; "Charlie"; "Alice"]
                    ["Alice"; "Charlie"; "Bob"; "Alice"] ] }
            test "direct neighbours" {
                applyRule ["Alice"; "Bob"; "X"] ("Alice", "Bob", 1) =! 1
                applyRule ["X"; "Bob"; "Alice"] ("Alice", "Bob", 1) =! 1 }
            test "no neighbours" {
                applyRule ["Alice"; "X"] ("Alice", "Bob", 1) =! 0 }
            test "neighbours at end of list" {
                applyRule ["Alice"; "X"; "Bob"] ("Alice", "Bob", 1) =! 1
                applyRule ["Bob"; "X"; "Alice"] ("Alice", "Bob", 1) =! 1 }
            test "solve" { solve demoinput =! 330 }
        ]
    ]


