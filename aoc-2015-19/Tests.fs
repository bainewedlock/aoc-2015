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
            test "example 1" {
                replace [("H",["H";"O"])] ["H";"O";"H"] |> Set =! (Set [
                    ["H"; "O"; "O"; "H"]
                    ["H"; "O"; "H"; "O"] ]) }
            test "example 5" {
                replace ["O",["H";"H"]] ["H";"O";"H"] |> Set =! Set [
                    ["H"; "H"; "H"; "H"]] }
            test "all combinations" {
                let rules = [
                    "H", ["H";"O"]
                    "H", ["O";"H"]
                    "O", ["H";"H"] ]
                replace rules ["H";"O";"H"] |> Set =! (Set [
                    ["H";"O";"O";"H"]
                    ["H";"O";"H";"O"]
                    ["O";"H";"O";"H"]
                    ["H";"H";"H";"H"] ]) }
            test "parsing" {
                parse "Mg => TiMg" =! ("Mg", ["Ti";"Mg"]) }
            test "splitMolecule" {
                splitMolecule "AbCd" =! ["Ab"; "Cd"]
                splitMolecule "ACd" =! ["A"; "Cd"] } ]
        testList "part 2" [
            test "parsing" {
                parse2 "Mg => TiMg" =! ("TiMg", "Mg") }
            test "example" {
                let rules = [
                    "H", "e"
                    "O", "e"
                    "HO", "H"
                    "OH", "H"
                    "HH", "O" ]
                reduce rules "e" "HOH" =! 3
                reduce rules "e" "HOHOHO" =! 6 } ]
    ]


