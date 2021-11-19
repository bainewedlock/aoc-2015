module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "examples" {
                solve ">" =! 2
                solve "^>v<" =! 4
                solve "^v^v^v^v^v" =! 2 }
            test "tracking" {
                track [1,0] ['^'] =! [1,1; 1,0]
                track [4,4] ['v'] =! [4,3; 4,4]
                track [0,0] ['>'] =! [1,0; 0,0]
                track [5,3] ['<'] =! [4,3; 5,3]
                track [0,0] ['^'; '<'] =! [-1,1; 0,1; 0,0]
                track [0,0] ['<'; '>'] =! [0,0; -1,0; 0,0] } ]
        testList "part 2" [
            test "examples" {
                solve2 "^v" =! 3
                solve2 "^v^v^v^v^v" =! 11 }
            test "split" {
                partitionIndexed
                    (fst >> even)
                    ['^'; 'v'; '>'] =! [['^'; '>']; ['v']] }
        ]
    ]


