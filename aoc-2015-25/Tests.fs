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
            test "order" {
                order () |> Seq.take 6 |> Seq.toList =!
                    [ 1,1; 2,1; 1,2; 3,1; 2,2; 1,3 ] }
            test "example" {
                solvePos (1,1) =! 20151125
                solvePos (6,6) =! 27995004 }
            test "parse" {
                parse "To continue, please consult the code grid in the manual.  Enter the code at row 10, column 20."
                    =! (10, 20) }
        ]
    ]


