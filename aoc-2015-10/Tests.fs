module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "toCounts" {
                toCounts "123" =! [1,1; 1,2; 1,3] }
            test "compress" {
                compress [1,1; 3,1; 1,3] =! [4,1; 1,3] }
            test "example" {
                nextItem 4 [1,1] |> compress =! [3,1; 2,2; 1,1]}
        ]
    ]


