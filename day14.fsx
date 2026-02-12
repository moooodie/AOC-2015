let input = 
    System.IO.File.ReadAllLines "input.txt"

//Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
type Reindeer = {
    Speed: int
    FlyTime: int
    RestTime: int
}

open System

let reindeers = 
    input
    |> Array.map (fun str ->
        let strs = 
            str.Split ' '
            |> Array.filter (fun word ->
                Char.IsDigit(word[0]))
                
        
        {
            Speed = int strs[0]
            FlyTime = int strs[1]
            RestTime = int strs[2]
        })

let partOne = 
    reindeers
    |> Array.map (fun r ->
        let sessionTime = r.FlyTime + r.RestTime
        let distPerSession = r.FlyTime * r.Speed
        
        let totalSessions = 2503 / sessionTime
        let remainderSecs = min (2503 % sessionTime) r.FlyTime
        
        totalSessions * distPerSession + remainderSecs * r.Speed)

    |> Array.max

let newReindeers = 
    reindeers
    |> Array.map (fun r ->
        r, 0, false, r.FlyTime, 0)


let partTwo = 
    seq {1 .. 2503}
    |> Seq.fold (fun rArr _ ->
        let newRArr = 
            rArr
            |> Array.map (
                fun (r, traveledDist, isResting, remTime, points) ->
                    let newTD = traveledDist + if isResting then 0 else r.Speed
                    let newIR = if remTime = 1 then not isResting else isResting
                    
                    let newRemTime = 
                        if remTime <> 1 then 
                            remTime - 1
                        elif newIR then
                            r.RestTime
                        else r.FlyTime
                        
                    r, newTD, newIR, newRemTime, points)

        let curMaxDist = 
            newRArr
            |> Array.map (fun (_, tD, _, _, _) -> tD)
            |> Array.max

        newRArr
        |> Array.map(fun (r, tD, iR, remTime, p) ->
            
            r, tD, iR, remTime, p + if tD = curMaxDist then 1 else 0)
        ) newReindeers

    |> Array.maxBy (fun (_, _, _, _, p) -> p)
    |> (fun (_, _, _, _, p) -> p)