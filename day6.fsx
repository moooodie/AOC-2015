let input = System.IO.File.ReadAllLines "input.txt"

open System


let (| Toggle | TurnOff | TurnOn | ) (ins: string) = 
    let parts = ins.Split ' '

    let x1, y1 =
        let coords = parts[parts.Length - 3].Split ','

        int coords[0], int coords[1]

    let x2, y2 =
        let coords = parts[parts.Length - 1].Split ','

        int coords[0], int coords[1]

    if parts[0] = "toggle" then
        Toggle (x1, y1, x2, y2)
    elif parts[1] = "on" then
        TurnOn (x1, y1, x2, y2)
    else
        TurnOff (x1, y1, x2, y2)


let partOne = 
    input
    |> Array.fold (
        fun (grid: int array array) inst ->

            match inst with
            | Toggle (x1, y1, x2, y2) ->

                for x in x1..x2 do
                    for y in y1..y2 do 
                        grid[y][x] <- if grid[y][x] = 1 then 0 else 1

            | TurnOn (x1, y1, x2, y2) ->
                for x in x1..x2 do
                    for y in y1..y2 do 
                        grid[y][x] <- 1


            | TurnOff (x1, y1, x2, y2) ->
                for x in x1..x2 do
                    for y in y1..y2 do 
                        grid[y][x] <- 0

            grid) (Array.init 1000 (fun _ -> Array.init 1000 (fun _ -> 0 )))

    |> Array.sumBy Array.sum


let partTwo = 
    input
    |> Array.fold (
        fun (grid: uint64 array array) inst ->

            match inst with
            | Toggle (x1, y1, x2, y2) ->

                for x in x1..x2 do
                    for y in y1..y2 do 
                        grid[y][x] <- grid[y][x] + 2UL

            | TurnOn (x1, y1, x2, y2) ->
                for x in x1..x2 do
                    for y in y1..y2 do 
                        grid[y][x] <- grid[y][x] + 1UL


            | TurnOff (x1, y1, x2, y2) ->
                for x in x1..x2 do
                    for y in y1..y2 do 
                        grid[y][x] <- if grid[y][x] = 0UL then 0UL else grid[y][x] - 1UL 

            grid) (Array.init 1000 (fun _ -> Array.init 1000 (fun _ -> 0UL )))

    |> Array.sumBy Array.sum