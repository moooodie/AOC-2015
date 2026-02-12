let input = 
    System.IO.File.ReadAllLines "input.txt"

type Ingredient = {
    Capacity: int
    Durability: int
    Flexibility: int
    Texture: int
    Calories: int
}

with
    member r.Item str =
        if str = "cap" then
            r.Capacity
        elif str = "dur" then
            r.Durability
        elif str = "flex" then
            r.Flexibility
        elif str = "tex" then
            r.Texture
        else r.Calories

let convToInt (str: string) = 
    try
        Some (int str)
    with
    | _ -> None

let rec generate (curList: int list) max listCount = 
    if curList.Length = listCount - 1 then
        [ (max :: curList) ]
    else
        [
            for i in 0 .. max do 
                yield! generate (i :: curList) (max - i) listCount
        ]

let ingredients = 
    input
    |> Array.map (fun str ->
        let strs = 
            str.Split [|' '; ',' |]
            |> Array.choose convToInt
        
        {
            Capacity = int strs[0]
            Durability = int strs[1]
            Flexibility = int strs[2]
            Texture = int strs[3]
            Calories = int strs[4]
        })

let words = [| "cap"; "dur"; "flex"; "tex" |]
let arrangements = generate [] 100 ingredients.Length
let data = 
    arrangements
    |> List.map (fun amounts ->
        let totalScore = 
            words
            |> Array.fold (fun acc property ->

                if acc = 0 then 
                    0
                else
                    amounts
                    |> List.indexed
                    |> List.sumBy (fun (index, amount) ->
                        ingredients[index][property] * amount)
                    |> fun num ->
                    if num < 0 then
                        0
                    else num * acc
                ) 1
        
        let calorieScore = 
            amounts
            |> List.indexed
            |> List.sumBy (fun (index, amount) ->
                ingredients[index]["cal"] * amount)
        
        totalScore, calorieScore)

let partOne = 
    data
    |> List.map (fun (totalScore, _) -> totalScore)
    |> List.max

let partTwo = 
    data
    |> List.filter (fun (_, cals) -> cals = 500)
    |> List.map (fun (totalScore, _) -> totalScore)
    |> List.max