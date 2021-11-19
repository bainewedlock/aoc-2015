module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "part1" [
            test "examples" {
                solve "[1,2,3]" =! 6
                solve "[-1,{\"a\":1}]" =! 0 } ]
        testList "part2" [
            test "examples" {
                solve2 "[1,{\"c\":\"red\",\"b\":2},3]" =! 4
                solve2 "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" =! 0
                solve2 "[1,\"red\",5]" =! 6
                solve2 "{\"a\":{\"c\":\"red\",\"b\":2},\"x\":3}" =! 3
            }
        ]
    ]


