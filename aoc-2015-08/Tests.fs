module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "empty string" { eval "\"\"" =! 0 }
            test "abc" { eval "\"abc\"" =! 3}
            test "escape" { eval "aaa\"\\\"aaa"=! 6 }
            test "code"  { eval "\"\\x27\"" =! 1 }
            test "backslash"  { eval "\"\\\\\"" =! 1 }
            test "example" { solve "DemoInput.txt" =! 12 } ]
        testList "part 2" [
            test "empty string" { encode "\"\"" =! 6}
            test "abc" { encode "\"abc\"" =! 9 }
            test "quote" { encode "\"aaa\\\"aaa\"" =! 16 }
            test "code" { encode "\"\\x27\"" =! 11 }
            test "example" { solve2 "DemoInput.txt" =! 19 } ]
    ]


