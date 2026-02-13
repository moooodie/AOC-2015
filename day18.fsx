type Coord = { State: bool; X: int; Y: int }

let input = System.IO.File.ReadAllLines "input.txt"

let xMax, yMax = input[0].Length - 1, input.Length - 1

let lights =
    input
    |> Array.indexed
    |> Array.collect (fun (y, str) ->
        str.ToCharArray()
        |> Array.indexed
        |> Array.map (fun (x, char) ->{ State = char = '#'; X = x; Y = y } ))

let neighboursDef xMax yMax x y = 
    [|
        x - 1, y - 1; x, y - 1; x + 1, y - 1 
        x - 1, y; x + 1, y
        x - 1, y + 1; x, y + 1; x + 1, y + 1
    |]
    |> Array.filter (fun (x, y) -> x >= 0 && x <= xMax && y >= 0 && y <= yMax)

let neighbours = neighboursDef xMax yMax

let partOne = 
    seq { 1 .. 100 }
    |> Seq.fold ( fun (lights: Coord array) _ ->

        let ons = 
            lights
            |> Seq.filter (fun { State = state} -> state)
            |> Seq.map (fun { X = x; Y = y} -> x, y)
            |> Array.ofSeq

        lights
        |> Array.map (fun coord ->
            let len = 
                neighbours coord.X coord.Y
                |> Seq.filter (fun x -> ons |> Array.contains x)
                |> Seq.length
            
            {
                coord with
                    State = 
                        match coord.State with
                        | true -> len = 2 || len = 3
                        | false -> len = 3        
            })
        ) lights
    |> Seq.filter (fun { State = s} -> s)
    |> Seq.length

let corners = 
    [|
        0, 0
        xMax, 0
        0, yMax
        xMax, yMax
    |]
let partTwo = 
    seq { 1 .. 100 }
    |> Seq.fold ( fun (lights: Coord array) _ ->

        let ons = 
            lights
            |> Seq.filter (fun { State = state} -> state)
            |> Seq.map (fun { X = x; Y = y} -> x, y)
            |> Set
            |> fun set ->
               corners
               |> Array.fold (fun s c ->
                   s |> Set.add c) set

        lights
        |> Array.map (fun coord ->
            if corners |> Array.contains (coord.X, coord.Y) then
                { coord with State = true }
            else
                let len = 
                    neighbours coord.X coord.Y
                    |> Seq.filter (fun x -> ons |> Set.contains x)
                    |> Seq.length
                
                {
                    coord with
                        State = 
                            match coord.State with
                            | true -> len = 2 || len = 3
                            | false -> len = 3        
                })
        ) lights

    |> Seq.filter (fun { State = s} -> s)
    |> Seq.length