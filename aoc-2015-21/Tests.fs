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
            test "combinations" {
                let stock = [
                    Weapon, 8
                    Armor, 13
                    Ring, 25 ]
                combinations stock
                |> Seq.length =! 7 }
            test "combinations with weapon" {
                let stock = [
                    Weapon, 8
                    Armor, 13
                    Ring, 25 ]
                validCombinations stock
                |> Seq.length =! 4 }
            test "parseShop" {
                parseShop @"
Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Warhammer    25     6       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
                " =! Map [
                    (Weapon, 8), (4, 0)
                    (Weapon, 25), (6, 0)
                    (Armor, 13), (0, 1) ] }
            test "combat" {
                let player = {
                    hitPoints = 8
                    damage = 5
                    armor = 5 }
                let enemy = {
                    hitPoints = 12
                    damage = 7
                    armor = 2 }
                let state = CombatState.create player enemy 
                let state2 = CombatState.turn state
                state2.current.hitPoints =! 9
                state2.turnIndex =! 1
                let state3 = CombatState.turn state2
                state3.current.hitPoints =! 6
                state3.turnIndex =! 2

                let result = CombatState.resolve state
                result.current.hitPoints =! 0
                result.turnIndex =! 7

                CombatState.playerWins state =! true }
            test "create player" {
                let state = {
                    purchased = [
                        Weapon, 10
                        Weapon, 20 ]
                    stock = [] }
                let shop = Map [
                    (Weapon, 10), (4, 0)
                    (Weapon, 20), (6, 0)
                ]
                let p = Player.create 100 state shop
                p.hitPoints =! 100
                p.damage =! 10
            }
        ]
    ]


