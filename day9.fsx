let input = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map (fun str -> str.Split ' ')

let locations = 
    input
    |> Seq.collect(fun strArr -> seq { strArr[0]; strArr[2] })
    |> Seq.distinct
    |> List.ofSeq

let edges = 
    input
    |> Array.map (fun strArr ->
        (strArr[0], strArr[2]), int strArr[strArr.Length - 1])
    |> Map.ofArray

let rec permutations (list: 'a list) = 
    if list.Length = 0 then
        [ [ ] ]

    else
        [
            for i in 0 .. list.Length - 1 do
                let rest = 
                    list
                    |> List.mapi(fun j x -> j, x)
                    |> List.filter (fun (j, _) -> j <> i)
                    |> List.map snd

                
                for p in permutations rest do
                    yield list[i] :: p
        ]

let allPaths = permutations locations

let pathCost (path: string list) : int option = 

    path
    |> List.tail
    |> List.fold (fun (prevStr, prevCost) curStr ->
        match prevCost with

        | None -> curStr, None

        | Some pC ->
            let curPathCost = 
                edges
                |> Map.tryPick(fun (str1, str2) cost ->
                    if str1 = prevStr && str2 = curStr || str1 = curStr && str2 = prevStr then
                        Some cost
                    else None)

            match curPathCost with
            | Some num ->
                curStr, Some (num + pC)
            | _ -> curStr, None

    ) (path[0], Some 0)
    |> snd


let partOne = 
    allPaths
    |> List.choose pathCost
    |> List.min

let partTwo = 
    allPaths
    |> List.choose pathCost
    |> List.max