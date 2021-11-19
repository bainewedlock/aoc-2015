module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "step 1" [
            test "parse" {
                parse ("Rudolph can fly 22 km/s for 8 seconds, " 
                    + "but then must rest for 165 seconds.")
                    =! ("Rudolph", 22, 8, 165) }
            test "examples" {
                distanceAfter 1000 ("", 14, 10, 127) =! 1120 } ]
        testList "step 2" [
            test "reindeer movement" {
                let r = Reindeer("Comet", 10, 2, 1)
                r.name =! "Comet"
                r.distance =! 0
                r.move()
                r.distance =! 10
                r.move()
                r.distance =! 20
                r.move()
                r.distance =! 20
                r.move()
                r.distance =! 30 }
            test "scoring" {
                let r = Reindeer("Comet", 0, 0, 0)
                r.score =! 0
                r.addPoint()
                r.score =! 1 }
            test "one second" {
                let rs = [
                    Reindeer ("Dancer", 14, 10, 127)
                    Reindeer ("Comet", 16, 11, 162) ]
                doMoveAll rs
                rs.[0].score =! 0
                rs.[1].score =! 1 }
        ]
    ]

