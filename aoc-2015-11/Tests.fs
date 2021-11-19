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
            test "rule 1" {
                rule1 [1;2;3] =! true
                rule1 [3;1;2;3] =! true
                rule1 [3;1;2;2;3] =! false }
            test "rule 2" {
                rule2 ['i'] =! false
                rule2 ['o'] =! false
                rule2 ['l'] =! false
                rule2 ['x'] =! true }
            test "rule 3" {
                rule3 ["a";"a";"b";"b"] =! true
                rule3 ["a";"a";"b";"a";"a"] =! false }
            test "inc" {
                inc ['a';'a';'z'] =! ['a';'b';'a']
                inc ['y';'z';'z'] =! ['z';'a';'a'] }
            test "example" {
                let chars = "abcdffaa" |> Seq.toList
                rule1 (chars |> List.map int) =! true
                rule2 chars =! true
                rule3 chars =! true
                check chars =! true

                solve "abcdefgh" =! "abcdffaa"
            }
        ]
    ]


