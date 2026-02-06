let input = 
    let temp = System.IO.File.ReadAllLines "input.txt" in
    temp[0]


let solver count = 
    seq {0..(count - 1)}

    |> Seq.fold(fun (str: string) _ ->

        seq { yield! str; yield 'a' }

        |> Seq.tail

        |> Seq.fold ( fun (prevChar, count, (curList: ResizeArray<char>)) curChar ->
            
            if prevChar <> curChar then
                let countToAdd = string count

                curList.AddRange (seq { yield! countToAdd; yield prevChar })

                curChar, 1, curList
            else
                curChar, count + 1, curList
                
        ) (str[0], 1, ResizeArray<char>() )
        
        |> (fun (_, _, z) -> z)
        |> Array.ofSeq
        |> System.String
    ) input
    |> _.Length

let partOne = solver 40
let partTwo = solver 50