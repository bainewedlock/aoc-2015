module Solution

open System
type ItemClass = Weapon | Armor | Ring

type StockUnit = ItemClass * int
type WeaponStats = int * int
type Shop = Map<StockUnit, WeaponStats>

type ShoppingState = {
    purchased : StockUnit List
    stock : StockUnit List }

module ShoppingState =
    let create stock = {
        purchased = []
        stock = Seq.toList stock }

type Player = {
    hitPoints : int
    damage : int
    armor : int }

module Player = 
    let create hitPoints (state:ShoppingState) (shop:Shop) =
        let damage, armor =
            state.purchased
            |> List.map (fun x -> shop.Item x)
            |> List.reduce (fun (d1,a1) (d2,a2) -> d1+d2,a1+a2)
        {
            hitPoints = hitPoints
            damage = damage
            armor = armor
        }

type CombatState = {
    current : Player
    other : Player
    turnIndex : int }

module CombatState =
    let create player enemy = {
        current = player
        other = enemy
        turnIndex = 0 }
    let turn (s:CombatState) =
        let damage =
            s.current.damage - s.other.armor
            |> max 1
        { s with
            turnIndex = s.turnIndex + 1
            other = s.current
            current =
                { s.other with hitPoints = s.other.hitPoints - damage } }
    let rec resolve (s:CombatState) =
        if s.current.hitPoints <= 0 then s else
        turn s |> resolve
    let playerWins s = (resolve s).turnIndex % 2 = 1

let combinations stock =
    let initialState = ShoppingState.create stock
    let rec loop state = [
        if state.stock <> [] then
            let item = state.stock.Head
            let state' = {
                state with
                    purchased = item::state.purchased
                    stock = state.stock.Tail }
            let state'' = { state with stock = state.stock.Tail }
            yield state'
            yield! loop state'
            yield! loop state'' ]
    loop initialState

let validCombinations stock =
    combinations stock
    |> List.filter (fun x ->
        let types = x.purchased |> List.countBy fst |> Map
        (types.TryFind Weapon |> Option.defaultValue 0) = 1
        && (types.TryFind Armor  |> Option.defaultValue 0) <= 1
        && (types.TryFind Ring   |> Option.defaultValue 0) <= 2)

let specialSplit (s:string) =
    s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let parseShop (shop:string) : Shop =
    shop.Split "\r\n"
    |> Seq.map (fun x -> x.Replace(" +", ""))
    |> Seq.fold (fun (nextClass, items) x ->
        match specialSplit x with
        | "Weapons:"::_ -> (Weapon, items)
        | "Armor:"  ::_ -> (Armor,  items)
        | "Rings:"  ::_ -> (Ring,   items)
        | _::price::damage::armor::_ ->
            let item = (nextClass, int price), (int damage, int armor)
            (nextClass, item::items)
        | _             -> (nextClass, items))
        (Weapon,[])
    |> snd
    |> List.rev
    |> Map<StockUnit, WeaponStats>

let genericSolve fFilter shopDescription =
    let shop = parseShop shopDescription
    let enemy = { hitPoints = 109; damage = 8; armor = 2 }
    shop
    |> Seq.map (fun x -> x.Key)
    |> validCombinations
    |> Seq.filter (fun x ->
        let p = Player.create 100 x shop
        let s = CombatState.create p enemy
        fFilter s)
    |> Seq.map (fun x -> x.purchased |> List.sumBy snd)

let solve = genericSolve CombatState.playerWins >> Seq.min
let solve2 = genericSolve (CombatState.playerWins >> not) >> Seq.max
