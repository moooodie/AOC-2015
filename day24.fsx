let packages = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map int

let generateAllValidGroups (packages: int array) targetWeight = 

    let rec recurse (curPackages: int list) curIndex = 

        if curPackages |> List.sum = targetWeight then
            [| curPackages |]

        elif curPackages |> List.sum > targetWeight then
            Array.empty
        else
            [|
                for i in curIndex .. (packages.Length - 1) do
                    yield!
                        recurse (packages[i] :: curPackages) (i + 1)
                        |> Array.filter (fun l -> l.Length <> 0)
            |]

    recurse [] 0

let partOneGroups = 
    generateAllValidGroups packages ((packages |> Array.sum) / 3)

let partOneMinGroupLength = 
    partOneGroups
    |> Array.map (fun l -> l.Length)
    |> Array.min

let partOne = 
    partOneGroups
    |> Array.filter (fun l -> l.Length = partOneMinGroupLength)
    |> Array.map (fun l -> l |> List.fold (fun n i -> n * int64 i) 1L)
    |> Array.min

let partTwoGroups = 
    generateAllValidGroups packages ((packages |> Array.sum) / 3)

let partTwoMinGroupLength = 
    partTwoGroups
    |> Array.map (fun l -> l.Length)
    |> Array.min

let partTwo = 
    partTwoGroups
    |> Array.filter (fun l -> l.Length = partTwoMinGroupLength)
    |> Array.map (fun l -> l |> List.fold (fun n i -> n * int64 i) 1L)
    |> Array.min