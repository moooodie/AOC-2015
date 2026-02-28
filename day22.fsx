[<Struct>]
type Boss = {
    HitPoints: int
    Damage: int
}

[<Struct>]
type Player = {
    HitPoints: int
    ManaPoints: int
    Armor: int
}

type Effect = 
    | Shield of Turns: int
    | Poison of Turns: int
    | Recharge of Turns: int


let convToInt (str: string) = 
    try
        Some (int str)
    with
    | _ -> None

let spells = 
    [|
        "Magic Missile"; "Drain"; "Shield"
        "Poison"; "Recharge"
    |]

let notAnActiveEffect (activeEffects: Effect list) (spell: string) = 
    activeEffects
    |> List.fold (fun isActive -> 
        function
        | Shield _ when spell = "Shield" -> false
        | Poison _ when spell = "Poison" -> false
        | Recharge _ when spell = "Recharge" -> false
        | _ -> isActive
        ) true

let boss = 
    let arr = 
        System.IO.File.ReadAllLines "input.txt" 
        |> Array.collect (fun line ->
            line.Split ' ')
        |> Array.choose convToInt

    {
        HitPoints = arr[0]
        Damage = arr[1]
    }

let player = {
    HitPoints = 50
    ManaPoints = 500
    Armor = 0
}

let partOne = 
    let mutable res = System.Int32.MaxValue

    let rec recurse (boss: Boss) player spentMana 
        (activeEffects: Effect list) isPlayerTurn = 

        if boss.HitPoints <= 0 then  
            res <- min res spentMana
            
        
        elif player.HitPoints <= 0 || player.ManaPoints <= 0 then
            ()
        else
            let (newBoss: Boss), newPlayer = 
                activeEffects
                |> List.fold (fun (boss: Boss, player: Player) ->
                    function
                    | Shield _ ->
                        boss, { player with Armor = 7 }
                    | Poison _ ->
                        { boss with HitPoints = boss.HitPoints - 3 }, player
                    | Recharge _ ->
                        boss, { player with ManaPoints = player.ManaPoints + 101 }
                    ) (boss, player)

            if newBoss.HitPoints <= 0 then
                res <- min res spentMana
            else
                let newEffects = 
                    activeEffects
                    |> List.map (
                        function
                        | Shield num -> Shield (num - 1)
                        | Poison num -> Poison (num - 1)
                        | Recharge num -> Recharge (num - 1))
                    |> List.filter (
                        function
                        | Shield n| Poison n | Recharge n -> n > 0)

                if not isPlayerTurn then
                    let damageDone = 
                        max 1 (newBoss.Damage - newPlayer.Armor)

                    recurse newBoss { newPlayer with Armor = 0; HitPoints = newPlayer.HitPoints - damageDone } spentMana
                        newEffects (not isPlayerTurn)

                else
                    let usableSpells =
                        spells
                        |> Array.filter (notAnActiveEffect newEffects)

                    usableSpells
                    |> Array.iter (fun spell ->
                        if spell = "Magic Missile" then
                            recurse { newBoss with HitPoints = newBoss.HitPoints - 4 } 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 53 }
                                (spentMana + 53) newEffects 
                                (not isPlayerTurn)

                        elif spell = "Drain" then
                            recurse { newBoss with HitPoints = newBoss.HitPoints - 2 } 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 73; HitPoints = newPlayer.HitPoints + 2 }
                                (spentMana + 73) newEffects 
                                (not isPlayerTurn)
                            
                        elif spell = "Shield" then
                            recurse newBoss 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 113 }
                                (spentMana + 113) ( Shield 6 :: newEffects)
                                (not isPlayerTurn)

                        elif spell = "Poison" then
                            recurse newBoss 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 173 }
                                (spentMana + 173) ( Poison 6 :: newEffects)
                                (not isPlayerTurn)

                        else
                            recurse newBoss 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 229 }
                                (spentMana + 229) ( Recharge 5 :: newEffects)
                                (not isPlayerTurn)
                            )
    recurse boss player 0 [] true
    res


let partTwo = 
    let mutable res = System.Int32.MaxValue

    let rec recurse (boss: Boss) player spentMana 
        (activeEffects: Effect list) isPlayerTurn = 

        if boss.HitPoints <= 0 then 
            res <- min res spentMana
            
        
        elif player.HitPoints <= 0 || player.ManaPoints <= 0 then
            ()
        else
        
            let (newBoss: Boss), tempPlayer = 
                activeEffects
                |> List.fold (fun (boss: Boss, player: Player) ->
                    function
                    | Shield _ ->
                        boss, { player with Armor = 7 }
                    | Poison _ ->
                        { boss with HitPoints = boss.HitPoints - 3 }, player
                    | Recharge _ ->
                        boss, { player with ManaPoints = player.ManaPoints + 101 }
                    ) (boss, player)

            let newPlayer = 
                { tempPlayer with 
                    HitPoints = 
                        if isPlayerTurn then
                            tempPlayer.HitPoints - 1
                        else tempPlayer.HitPoints }

            
            if newPlayer.HitPoints <= 0 then
                ()
            elif newBoss.HitPoints <= 0 then
                res <- min res spentMana
            else
                let newEffects = 
                    activeEffects
                    |> List.map (
                        function
                        | Shield num -> Shield (num - 1)
                        | Poison num -> Poison (num - 1)
                        | Recharge num -> Recharge (num - 1))
                    |> List.filter (
                        function
                        | Shield n| Poison n | Recharge n -> n > 0)

                if not isPlayerTurn then
                    let damageDone = 
                        max 1 (newBoss.Damage - newPlayer.Armor)

                    recurse newBoss { newPlayer with Armor = 0; HitPoints = newPlayer.HitPoints - damageDone } spentMana
                        newEffects (not isPlayerTurn)

                else
                    let usableSpells =
                        spells
                        |> Array.filter (notAnActiveEffect newEffects)

                    usableSpells
                    |> Array.iter (fun spell ->
                        if spell = "Magic Missile" then
                            recurse { newBoss with HitPoints = newBoss.HitPoints - 4 } 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 53 }
                                (spentMana + 53) newEffects 
                                (not isPlayerTurn)

                        elif spell = "Drain" then
                            recurse { newBoss with HitPoints = newBoss.HitPoints - 2 } 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 73; HitPoints = newPlayer.HitPoints + 2 }
                                (spentMana + 73) newEffects 
                                (not isPlayerTurn)
                            
                        elif spell = "Shield" then
                            recurse newBoss 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 113 }
                                (spentMana + 113) ( Shield 6 :: newEffects)
                                (not isPlayerTurn)

                        elif spell = "Poison" then
                            recurse newBoss 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 173 }
                                (spentMana + 173) ( Poison 6 :: newEffects)
                                (not isPlayerTurn)

                        else
                            recurse newBoss 
                                { newPlayer with Armor = 0; ManaPoints = newPlayer.ManaPoints - 229 }
                                (spentMana + 229) ( Recharge 5 :: newEffects)
                                (not isPlayerTurn)
                            )
    recurse boss player 0 [] true
    res