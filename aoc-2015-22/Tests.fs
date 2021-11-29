module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "solution" { solve "" =! 1269 } ]
        testList "part 2" [
            test "solution" { solve2 "" =! 1309 } ]
    ]


