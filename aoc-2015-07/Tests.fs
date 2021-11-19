module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput
open System

[<Tests>]
let all =
    testList "all" [
        testList "parsing" [
            test "from input" {
                parseLine "bo OR bu -> bv" =! ("bv", OR ("bo", "bu"))
                parseLine "ip LSHIFT 15 -> it" =! ("it", LSHIFT ("ip", "15"))
                parseLine "lx -> a" =! ("a", Wire "lx")
                parseLine "lf AND lq -> ls" =! ("ls", AND ("lf", "lq"))
                parseLine "iu RSHIFT 1 -> jn" =! ("jn", RSHIFT ("iu", "1"))
                parseLine "NOT kt -> ku" =! ("ku", NOT "kt")
            }
            test "notify unknown line" {
                Expect.throwsT<ArgumentException>
                    (fun _ -> parseLine "blabla" |> ignore)
                    "did not throw exception" } ]
        testList "evaluation" [
            test "a signal needs no eval" {
                let s = toState [ "a", Number 1 ]
                doEval s "a"
                s.["a"] =! Number 1 }
            test "a wire gets forwarded" {
                let s =
                    toState [
                        "a", Number 1
                        "x", Wire "a" ]
                doEval s "x"
                s.["x"] =! Number 1 }
            test "forward multiple steps" {
                let s =
                    toState [
                        "a", Number 1
                        "b", Wire "a"
                        "x", Wire "b" ]
                doEval s "x"
                s.["x"] =! Number 1 }
            test "eval OR" {
                let s =
                    toState [
                        "a", Number 1
                        "b", Number 2
                        "x", OR ("a","b") ]
                doEval s "x"
                s.["x"] =! Number 3 }
            test "eval LSHIFT" {
                let s = toState [
                    "a", LSHIFT ("b", "1")
                    "b", Number 2 ]
                doEval s "a"
                s.["a"] =! Number 4 }
            //test "eval AND" {
            //}
        ]
    ]


