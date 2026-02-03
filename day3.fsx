let input = 
    let baz = System.IO.File.ReadAllLines "input.txt" in baz[0]

let partOne = 
    seq {
        yield 0, 0

        yield!
            input
            |> Seq.scan( fun (x, y) dir ->
                match dir with
                | '^' -> x, y - 1
                | 'v' -> x, y + 1
                | '<' -> x - 1, y 
                | '>' -> x + 1, y 
                | _ -> failwith "You shouldn't be here"
            ) (0, 0)
    }
    |> Seq.distinct
    |> Seq.length

let partTwo = 
    seq {
        yield 0, 0

        yield!
            input
            |> Seq.indexed
            |> Seq.scan (fun (x1, y1, x2, y2) (dirPos, dir) ->
                match dir with
                
                | '^' ->
                    if dirPos % 2 = 0 then
                        x1, y1 - 1, x2, y2
                    else x1, y1, x2, y2 - 1
                    
                | 'v' ->
                    if dirPos % 2 = 0 then
                        x1, y1 + 1, x2, y2
                    else x1, y1, x2, y2 + 1
                    
                | '<' ->
                    if dirPos % 2 = 0 then
                        x1 - 1, y1, x2, y2
                    else x1, y1, x2 - 1, y2
                    
                | '>' ->
                    if dirPos % 2 = 0 then
                        x1 + 1, y1, x2, y2
                    else x1, y1, x2 + 1, y2
                    
                | _ -> failwith "You Shouldn't Be Here Part 2") (0, 0, 0, 0)

            |> Seq.collect (fun (x1, y1, x2, y2) ->
                seq { yield x1, y1; yield x2, y2 })
    }
    |> Seq.distinct
    |> Seq.length