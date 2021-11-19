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
                parse "London to Dublin = 464"
                    =! ("London", "Dublin", 464) }
            test "demoinput" {
                solve demoinput =! 605 }
            test "possibleRoutes" {
                let xs =
                    demoinput
                    |> parseInput
                    |> possibleRoutes
                Seq.length xs =! 6 }
            test "legs" {
                let xs = calcLegs [ "a", "b", 1 ]
                xs =! Map [ ("a","b"), 1; ("b","a"), 1] }
            test "locations" {
                let xs = locations [ "a", "b", 1 ]
                xs =! Set [ "a"; "b" ] }
        ]
    ]


