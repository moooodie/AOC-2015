type Item = {
    Cost: int
    Damage: int
    Defense: int
}

type Player = {
    HitPoints: float
    Damage: int
    Armor: int
}


type Combination = {
    Weapon: Item
    Armor: Item option
    Rings: Item option
}

with
    member c.TotalCost = 
        let armorCost = 
            c.Armor 
            |> Option.fold (fun _ i ->
                i.Cost) 0

        let ringsCost = 
            c.Rings
            |> Option.fold (fun _ i ->
                i.Cost) 0

        armorCost + ringsCost + c.Weapon.Cost

    member c.Damage = 
        c.Rings
        |> Option.fold (fun s i ->
            i.Damage + s) c.Weapon.Damage

    member c.Defense = 
        let ringsDef = 
            c.Rings
            |> Option.fold (fun _ i ->
                i.Defense )0

        c.Armor
        |> Option.fold (fun s i ->
            i.Defense + s ) ringsDef

let convToInt (str: string) = 
    try
        Some (int str)
    with
    | _ -> None
    
let allWeapons = 
    [|
        { Cost = 8; Damage = 4; Defense = 0 }; { Cost = 10; Damage = 5; Defense = 0 }

        { Cost = 25; Damage = 6; Defense = 0 }; { Cost = 40; Damage = 7; Defense = 0 }

        { Cost = 74; Damage = 8; Defense = 0 }
    |]

let allArmorChoices = 
    [|
        None; Some {Cost = 13; Damage = 0; Defense = 1 }

        Some {Cost = 31; Damage = 0; Defense = 2 }; Some {Cost = 53; Damage = 0; Defense = 3 }

        Some {Cost = 75; Damage = 0; Defense = 4 }; Some {Cost = 102; Damage = 0; Defense = 5 }
    |]

let allRingChoices = 
    let allRings = 
        [|
            { Cost = 25; Damage = 1; Defense = 0 }; { Cost = 50; Damage = 2; Defense = 0 }

            { Cost = 100; Damage = 3; Defense = 0 }; { Cost = 20; Damage = 0; Defense = 1 }

            { Cost = 40; Damage = 0; Defense = 2 }; { Cost = 80; Damage = 0; Defense = 3 }
        |]

    [|
        yield None

        for i in allRings do
            yield Some i

        for i in 0 .. allRings.Length - 1 do
            for j in (i + 1) .. allRings.Length - 1 do
                let newItem = 
                    {
                        Cost = allRings[i].Cost + allRings[j].Cost
                        Damage = allRings[i].Damage + allRings[j].Damage
                        Defense = allRings[i].Defense + allRings[j].Defense
                    }

                yield Some newItem
    |]

let boss = 
    let input = System.IO.File.ReadAllLines "input.txt"

    let nums =
        input
        |> Array.collect (fun str -> str.Split ' ')
        |> Array.choose convToInt

    {
        HitPoints = float nums[0]
        Damage = nums[1]
        Armor = nums[2]
    }

let yourHP = 100.0

let allCombinations = 
    seq {

        for w in allWeapons do
            for a in allArmorChoices do
                for r in allRingChoices do 
                    yield {
                        Weapon = w
                        Armor = a
                        Rings = r
                    }
    }

let partOne = 
    allCombinations
    |> Seq.filter (fun c ->

        let roundsYouLast =
            let bossOffense = 
                max 1 (boss.Damage - c.Defense)
                
            ceil (yourHP / float bossOffense)
            
        let roundsBossLasts =
            let yourOffense = 
                max 1 (c.Damage - boss.Armor)
                
            ceil (boss.HitPoints / float yourOffense)
            
        roundsYouLast >= roundsBossLasts)

    |> Seq.map (fun c -> c.TotalCost)
    |> Seq.min

let partTwo = 
    allCombinations
    |> Seq.filter (fun c ->

        let roundsYouLast =
            let bossOffense = 
                max 1 (boss.Damage - c.Defense)
                
            ceil (yourHP / float bossOffense)
            
        let roundsBossLasts =
            let yourOffense = 
                max 1 (c.Damage - boss.Armor)
                
            ceil (boss.HitPoints / float yourOffense)
            
        roundsYouLast < roundsBossLasts)

    |> Seq.map (fun c -> c.TotalCost)
    |> Seq.max