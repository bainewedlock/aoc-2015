module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "tests" [
            test "example" {
                presents 9 10 999 =! [|10;30;40;70;60;120;80;150;130|] }
            test "solve" { solve "130" =! 8 }
        ]
    ]


