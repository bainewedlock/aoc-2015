module Solution

type Spell = {
    Name : string
    Cost : int
    Damage : int
    Heal : int
    Armor : int
    Mana : int
    Duration : int
    isInstant : bool }

type GameState = {
    part : int
    hp : int
    addHpPerTurn : int
    mana : int
    manaSpent : int
    best : int
    bossHp : int
    activeSpells : Spell list }

module Spell =
    let damage = List.sumBy (fun s -> s.Damage)
    let mana   = List.sumBy (fun s -> s.Mana)
    let apply s state2 =
        let extraDamage, heal, activeSpells3 = 
            if s.isInstant then s.Damage, s.Heal, state2.activeSpells
            else 0, 0, s :: state2.activeSpells
        { state2 with
            hp = state2.hp + heal
            mana = state2.mana - s.Cost
            activeSpells = activeSpells3
            bossHp = state2.bossHp - extraDamage
            manaSpent = state2.manaSpent + s.Cost }

let allSpells = 
    [ { Name = "MM"; Cost = 53;  Damage = 4; Heal = 0; Armor = 0; Mana = 0;   Duration = 0; isInstant = true}
      { Name = "DR"; Cost = 73;  Damage = 2; Heal = 2; Armor = 0; Mana = 0;   Duration = 0; isInstant = true}
      { Name = "SH"; Cost = 113; Damage = 0; Heal = 0; Armor = 7; Mana = 0;   Duration = 6; isInstant = false}
      { Name = "PO"; Cost = 173; Damage = 3; Heal = 0; Armor = 0; Mana = 0;   Duration = 6; isInstant = false}
      { Name = "RE"; Cost = 229; Damage = 0; Heal = 0; Armor = 0; Mana = 101; Duration = 5; isInstant = false} ]

let startTurn state =
    { state with
        bossHp = state.bossHp - Spell.damage state.activeSpells
        mana =   state.mana   + Spell.mana   state.activeSpells
        activeSpells = state.activeSpells
                       |> List.map (fun s -> {s with Duration = s.Duration-1})
                       |> List.filter (fun s -> s.Duration > 0) }

let possibleDecisions state =
    let activeSpells = state.activeSpells |> Seq.map (fun s -> s.Name) |> Set
    allSpells |> List.filter (fun s ->
        s.Cost <= state.mana
        && not (activeSpells.Contains s.Name))

let rec bossTurn state =
    let state = startTurn state
    if state.bossHp <= 0
    then
        state.manaSpent
    else
        let armor  = state.activeSpells |> List.sumBy (fun s -> s.Armor)
        let damage = max (9 - armor) 1
        let state = { state with hp = state.hp - damage }
        if state.hp <= 0
        then
            state.best
        else
            playerTurn state

// how to reduce this if/then/if/then madness?
and playerTurn state =
    let state = startTurn state
    if state.part = 2 && state.hp = 1
    then
        state.best
    else
        if state.bossHp <= 0
        then
            state.manaSpent
        else
            let state = { state with hp = state.hp + state.addHpPerTurn }
            possibleDecisions state
            |> List.fold (fun best spell ->
                let state = Spell.apply spell state
                if state.manaSpent >= best
                then
                    best
                else
                    if state.bossHp <= 0
                    then
                        min best state.manaSpent
                    else
                        min best ({ state with best = best } |> bossTurn)) state.best

let part1State = {
    hp = 50
    part = 1
    mana = 500
    addHpPerTurn = 0
    manaSpent = 0
    best = 999999
    bossHp = 58
    activeSpells = [] }

let solve (input:string) = playerTurn part1State
let solve2 (input:string) = playerTurn { part1State with part = 2; addHpPerTurn = -1 }
