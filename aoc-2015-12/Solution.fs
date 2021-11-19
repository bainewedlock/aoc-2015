module Solution

open System.Text.RegularExpressions
open Newtonsoft.Json.Linq


let solve (s:string) =
    Regex.Matches(s, "\-?\d+")
    |> Seq.sumBy (fun m -> int m.Value)

let hasRedProperty (t:JObject) =
    t.Properties()
    |> Seq.exists (fun p -> p.Value |> string |> (=)"red")

let solve2 (json:string) =
    let rec loop (t:JToken) = seq [
        match t with
        | :? JArray as a -> for x in a do yield! loop x
        | :? JValue as v when v.Type = JTokenType.Integer -> yield v
        | :? JValue -> ()
        | :? JProperty as p -> yield! loop p.Value
        | :? JObject as o when hasRedProperty o -> ()
        | :? JObject as o -> yield! o.Properties() |> Seq.collect loop ]
    loop (JToken.Parse json)
    |> Seq.sumBy int

