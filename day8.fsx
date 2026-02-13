let input = 
    System.IO.File.ReadAllLines "input.txt"

let partOne = 
    input
    |> Array.sumBy (fun str ->
        let inMemoryCount = 
            str
            |> Seq.fold (fun (escapeCount, count) letter ->
                match escapeCount, letter with
                | 0, '\\' -> 1, count + 1
                | 0, _ -> 0, count + 1
                | 1, ('"' | '\\') -> 0, count
                | 1, 'x' -> 2, count
                | 1, _ -> failwith "can only have x, \\ or \" after first slash"

                | num, _ ->
                    if num = 3 then
                        0, count
                    else num + 1, count

                ) (0, 0)
            |> fun (_, c) -> c - 2

        str.Length - inMemoryCount)


let partTwo = 
    input
    |> Array.sumBy (fun str ->
        let requiredLiteralStringLength = 
           str
           |> Seq.sumBy(
                function
                | '"' | '\\' -> 2
                | _ -> 1)

        2 + requiredLiteralStringLength - str.Length)