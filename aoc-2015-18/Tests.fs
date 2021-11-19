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
            test "parsing" {
                let lights = parse demoinput
                Seq.length lights =! 6*6
                (lights.Item (0,0)).isOn =! false
                (lights.Item (1,0)).isOn =! true
                (lights.Item (1,0)).calculate ()
                (lights.Item (1,0)).isOn =! true
                (lights.Item (1,0)).tick ()
                (lights.Item (1,0)).isOn =! false
                (lights.Item (0,0)).debugNeighbours =! 3
                (lights.Item (1,0)).debugNeighbours =! 5
            }
            test "calculate" {
                let l = Light('#')
                l.calculate ()
                l.tick ()
                l.isOn =! false }

            test "stayOn" {
                let l = Light('.')
                l.stayOn ()
                l.isOn =! true
                l.calculate ()
                l.tick ()
                l.isOn =! true }
        ]
    ]


