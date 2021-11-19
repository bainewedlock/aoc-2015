module Tests


// https://fscheck.github.io/FsCheck/QuickStart.html

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open FsCheck
open Demoinput


type RecipeDefGen() =
    static member RecipeConstraint() : Arbitrary<RecipeDef> =
        let createCombo ingredients units =
            {
                ingredients = ingredients
                totalUnits = units
            }
        let getIngredientCount = Gen.choose(2,3)
        let getTotalUnits = Gen.choose(5,5)
        createCombo <!> getIngredientCount <*> getTotalUnits |> Arb.fromGen

[<AutoOpen>]
module Auto =
    let private config =
        let def = FsCheckConfig.defaultConfig
        { def with arbitrary = (typeof<RecipeDefGen>)::def.arbitrary }
    let testProp name = testPropertyWithConfig config name

[<Tests>]
let all =
    testList "all" [
        testList "part 1" [
            test "parsing" {
                let n, d =
                    parse ("Sprinkles: capacity 2, durability 0, " 
                        + "flavor -2, texture 0, calories 3")
                n =! "Sprinkles"
                d.Count =! 5
                d.Item "capacity" =! 2
                d.Item "durability" =! 0
                d.Item "flavor" =! -2
                d.Item "texture" =! 0
                d.Item "calories" =! 3 } 
            testProp "sum equals total" <| fun (x:RecipeDef) ->
                combos x |> List.forall (
                    fun c -> x.totalUnits = (List.sum c))
            testProp "has all ingredients" <| fun (x:RecipeDef) ->
                combos x |> List.forall (
                    fun c -> x.ingredients = (List.length c))
            testProp "ingredients have all counts" <| fun (x:RecipeDef) ->
                for i in [0..x.ingredients-1] do
                    let cs = combos x
                    let values =
                        cs
                        |> List.map (fun x -> x.[i])
                        |> List.distinct
                        |> List.sort
                    values =! [0..x.totalUnits]
                combos x |> List.forall (
                    fun c -> x.ingredients = (List.length c))

            //test "2 ingredients, 100 units" {
            //    let cs = combos { ingredients = 2; totalUnits = 100 }
            //    sprintf "%A" cs =! ""
            //}

            test "eval combo" {
                let ingredients = [
                    dict [ "c", -1; "d", -2 ] 
                    dict [ "c", 2; "d", 3 ] ]
                eval noCalories ingredients [44;56] =! 68*80 }

            test "demoinput" {
                let ingredients = parseInput demoinput
                eval noCalories ingredients [44;56] =! 68*80*152*76
                eval noCalories ingredients [94;6] =!  0
                solve demoinput =! 62842880
            }
        ]
    ]


