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
                parse "Sue 32: pomeranians: 10, vizslas: 5, goldfish: 5"
                    =! (32, [ "pomeranians", 10
                              "vizslas", 5
                              "goldfish", 5 ]) }
            test "diffing" {
                diff rules_part1 [
                    "pomeranians", 10
                    "akitas", 0 ] =! [ "pomeranians" ] }
        ]
    ]


