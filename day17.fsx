let containers = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map int


let partOne = 
    let mutable count = 0
    let rec backtrack curSum curIndex = 
        if curSum = 150 then
            count <- count + 1 
        elif curSum > 150 then
            ()
        else
            for i in curIndex .. (containers.Length - 1) do
                backtrack (curSum + containers[i]) (i + 1)

    backtrack 0 0

    count

let partTwo = 
    let list = ResizeArray<int list>()

    let rec backtrack curSum curIndex curList = 

        if curSum = 150 then
            list.Add curList
        
        elif curSum > 150 then
            ()
        else
            for i in curIndex .. (containers.Length - 1) do
                backtrack (curSum + containers[i]) (i + 1) 
                    (containers[i] :: curList)

    backtrack 0 0 []

    let minCount = 
        list
        |> Seq.map (fun l -> l.Length)
        |> Seq.min

    list
    |> Seq.filter (fun l -> l.Length = minCount)
    |> Seq.length