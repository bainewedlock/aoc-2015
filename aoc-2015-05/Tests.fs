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
            test "example" {
                isNice "ugknbfddgicrmopn" =! true
                isNice "jchzalrnumimnmhp" =! false }
            test "count vowels" {
                countVowels "a" =! 1
                countVowels "aa" =! 2
                countVowels "xouvii" =! 4 }
            test "hasDoubleLetter" {
                hasDoubleLetter "abcddef" =! true
                hasDoubleLetter "abcdefa" =! false }
            test "hasBadString" {
                hasBadString "ab" =! true
                hasBadString "aa" =! false
                hasBadString "cd" =! true
                hasBadString "pq" =! true
                hasBadString "xy" =! true } ]
        testList "part 2" [
            test "example" {
                isNice2 "qjhvhtzxzqqjkmpb" =! true }
            test "findPair" {
                findPair "aaxaa" =! Some ("aa", 0, 3)
                findPair "aaa" =! None
                findPair "aaaa" =! Some ("aa", 0, 2)
                findPair "uxxbcdefgxx" =! Some ("xx", 1, 9) }
            test "findRepeat" {
                findRepeat "xyx" =! Some 'x'
                findRepeat "xyyx" =! None }
        ]
    ]


