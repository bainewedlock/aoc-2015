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
            test "wrap example presents" {
                wrap (2,3,4) =! 58
                wrap (1,1,10) =! 43 }
            test "parse a line" {
                parse "1x23x4" =! (1, 23, 4) }
        ]
        testList "part 2" [
            test "ribbons for examples" {
                ribbon (2,3,4) =! 34
                ribbon (1,1,10) =! 14 }
        ]
    ]


