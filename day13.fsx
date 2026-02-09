let input = 
    System.IO.File.ReadAllLines "input.txt"

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

open System

let relationships = 
    input
    |> Array.map (fun str ->
        let strs = str.Split ([| ' '; '.' |], StringSplitOptions.RemoveEmptyEntries) 
        
        (strs[0], strs[strs.Length - 1]), (int strs[3] * if strs[2] = "gain" then 1 else - 1))
    |> Map.ofArray


let allArrangement = 
    input
    |> Array.collect(fun str ->
        let strs = str.Split ([| ' '; '.' |], StringSplitOptions.RemoveEmptyEntries)
        
        [| strs[0]; strs[strs.Length - 1]|])
    |> Array.distinct
    |> List.ofArray
    |> permutations
    |> List.map Array.ofList

let partOne = 
    allArrangement
    |> List.map(fun arrangement ->
        arrangement
        |> Array.indexed
        |> Array.sumBy(fun (ind, name) ->
            let left = 
                arrangement[if ind = 0 then arrangement.Length - 1 else ind - 1]
            let right = 
                arrangement[if ind = arrangement.Length - 1 then 0 else ind + 1]
            
            match Map.tryFind(name, left) relationships, 
                Map.tryFind(name, right) relationships with
            
            | Some n1, Some n2 -> n1 + n2
            
            | _ -> failwith "Doesnt exist in dict")
            
        )
    |> List.max

let allArrangementPlusYou = 
    input
    |> Array.collect(fun str ->
        let strs = str.Split ([| ' '; '.' |], StringSplitOptions.RemoveEmptyEntries)
        
        [| strs[0]; strs[strs.Length - 1]|])
    |> Array.distinct
    |> List.ofArray
    |> (fun l -> "You" :: l)
    |> permutations
    |> List.map Array.ofList

let partTwo = 
    allArrangementPlusYou
    |> List.map(fun arrangement ->
        arrangement
        |> Array.indexed
        |> Array.sumBy(fun (ind, name) ->
            let left = 
                arrangement[if ind = 0 then arrangement.Length - 1 else ind - 1]
            let right = 
                arrangement[if ind = arrangement.Length - 1 then 0 else ind + 1]
            
            match Map.tryFind(name, left) relationships, 
                Map.tryFind(name, right) relationships with
            
            | Some n1, Some n2 -> n1 + n2
            | Some n, None
            | None, Some n -> n
            | _ -> 0))
    |> List.max