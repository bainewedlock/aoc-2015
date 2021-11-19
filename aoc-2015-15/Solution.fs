module Solution

open System.Collections.Generic

let makeTuple = function
    | [a;b] -> a,int b
    | x     -> failwithf "could not makeTuple for %A" x

let splitAt (c:char) (s:string) = s.Split c |> Array.toList

let trim (s:string) = s.Trim()

let parse (line:string) = 
    let tokens = line |> splitAt ':'
    let properties =
        tokens.[1]
        |> splitAt  ','
        |> Seq.map (trim >> splitAt ' ' >> makeTuple)
    (tokens.Head, dict properties)

type RecipeDef = {
    ingredients : int
    totalUnits : int }

let combos (rd:RecipeDef) =
    let rec loop combo unitsLeft ingredientsLeft = [
        if ingredientsLeft = 1 then
            yield unitsLeft::combo else
        for n in [0..unitsLeft] do
            yield! loop (n::combo) (unitsLeft-n) (ingredientsLeft-1) ]
    loop [] rd.totalUnits rd.ingredients

type Ingredients = IDictionary<string,int>

let evalIngredients (ingredients:Ingredients) f =
    ingredients |> Seq.map (fun x -> x.Key, f * x.Value) |> dict

let noCalories = (<>)"calories"
let onlyCalories = (=)"calories"

let eval filter (ingredients:Ingredients list) combo =
    List.zip ingredients combo
    |> List.map (fun (i,c) -> evalIngredients i c)
    |> Seq.concat
    |> Seq.groupBy (fun x -> x.Key)
    |> Seq.filter (fst >> filter)
    |> Seq.map snd
    |> Seq.map (Seq.sumBy (fun y -> y.Value))
    |> Seq.map (fun x -> if x < 0 then 0 else x)
    |> Seq.reduce (*)

let parseInput (input:string) =
    input.Split "\r\n"
    |> Array.toList
    |> List.map (parse >> snd)

let solveGeneric filter (input:string) =
    let ingredients = parseInput input
    let definition = {
        ingredients = ingredients.Length
        totalUnits = 100 }
    combos definition
    |> List.filter (filter ingredients)
    |> List.map (eval noCalories ingredients)
    |> List.max

let solve = solveGeneric (fun _ _ -> true)
let solve2 = solveGeneric (fun ing -> eval onlyCalories ing >> (=)500)
