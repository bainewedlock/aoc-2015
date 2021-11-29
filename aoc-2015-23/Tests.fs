module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

let comp = { a = 12; b = 0; pc = 5 }

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "hlf instruction" {
                parse "hlf a" =! HLF 'a'
                let c = exec (HLF 'a') comp
                c.a =! 6
                c.pc =! 6 }
            test "tpl instruction" {
                parse "tpl a" =! TPL 'a'
                let c = exec (TPL 'a') comp
                c.a =! 36
                c.pc =! 6 }
            test "inc instruction" {
                parse "inc a" =! INC 'a'
                let c = exec (INC 'a') comp
                c.a =! 13
                c.pc =! 6 }
            test "inc b" {
                parse "inc b" =! INC 'b'
                let c = exec (INC 'b') comp
                c.a =! 12
                c.b =! 1
                c.pc =! 6 }
            test "jmp instruction" {
                parse "jmp -2" =! JMP -2
                let c = exec (JMP -2) comp
                c.a =! 12
                c.pc =! 3 }
            test "jie instruction with jump" {
                parse "jie a, -3" =! JIE ('a', -3)
                let c = exec (JIE ('a', -3)) comp
                c.a =! 12
                c.pc =! 2 }
            test "jie instruction without jump" {
                let c = exec (JIE ('a', -3)) { comp with a = 11 }
                c.a =! 11
                c.pc =! 6 }
            test "jio instruction with jump" {
                parse "jio a, -3" =! JIO ('a', -3)
                let c = exec (JIO ('a', -3)) { comp with a = 1 }
                c.a =! 1
                c.pc =! 2 }
            test "jio instruction without jump" {
                let c = exec (JIO ('a', -3)) { comp with a = 3 }
                c.a =! 3
                c.pc =! 6 }
            test "demoinput" {
                let prog = parseInput demoinput
                prog.Count =! 4
                let result = runToEnd prog { a = 0; b = 0; pc = 0 }
                result.pc =! 4
                result.a =! 2 }
        ]
    ]


