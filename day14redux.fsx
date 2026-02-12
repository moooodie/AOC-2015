let input = 
    System.IO.File.ReadAllLines "input.txt"

type Mode = 
    | Resting
    | Flying 

    with
        static member Toggle = 
            function
            | Resting -> Flying
            | Flying -> Resting


type Reindeer = {
    Speed: int
    FlyTime: int
    RestTime: int

    //Stuff For PartTwo
    CurrentMode: Mode
    TraveledDistance: int
    RemainingTime: int
    Points: int
}

open System
let reindeers = 
    input
    |> Array.map (fun str ->
        let strs = 
            str.Split ' '
            |> Array.filter (fun word ->
                Char.IsDigit word[0])
                
        {
            Speed = int strs[0]; FlyTime = int strs[1] 
            RestTime = int strs[2]; CurrentMode = Flying
            TraveledDistance = 0; RemainingTime = int strs[1]
            Points = 0
        })

let time = 2503

let partOne = 
    reindeers
    |> Array.map (fun r ->
        let sessionTime = r.FlyTime + r.RestTime
        let distPerSession = r.FlyTime * r.Speed
        
        let totalSessions = time / sessionTime
        let remainderSecs = min (time % sessionTime) r.FlyTime
        
        totalSessions * distPerSession + remainderSecs * r.Speed)

    |> Array.max


let partTwo = 
    seq { 1 .. time }
    |> Seq.fold (fun rArr _ -> 
        
        let newArr = 
            rArr
            |> Array.map ( fun r ->     
                {
                    r with

                        TraveledDistance = 
                            match r.CurrentMode with
                            | Flying -> r.TraveledDistance + r.Speed
                            | _ -> r.TraveledDistance

                        CurrentMode = 
                            if r.RemainingTime = 1 then
                                Mode.Toggle r.CurrentMode
                            else 
                                r.CurrentMode

                        RemainingTime = 
                            if r.RemainingTime <> 1 then
                                r.RemainingTime - 1
                            else
                                match r.CurrentMode with
                                | Resting -> r.FlyTime
                                | _ -> r.RestTime
                })

        let curMax = 
            newArr
            |> Array.map (fun r -> r.TraveledDistance)
            |> Array.max

        newArr
        |> Array.map (fun r ->
            {
                r with
                    Points = 
                        if r.TraveledDistance = curMax then
                            r.Points + 1
                        else r.Points
            })
        ) reindeers

    |>  Array.maxBy (fun { Points = p} -> p)
    |> fun { Points =  p} -> p