let json = 
    let temp = System.IO.File.ReadAllLines "input.txt" in
    temp[0]

open System
open System.Text.Json
let partOne = 
    json
    |> Seq.fold (fun (isNegative, curNum, sum) curChar ->
        if not (Char.IsDigit curChar) then
            curChar = '-', 0, sum + int64 curNum * (if isNegative then -1L else 1)
        else
            isNegative, curNum * 10 + int (curChar - '0'), sum
    ) (false, 0, 0L)
    |> fun (_, _, sum) -> sum

let rec sumJson (el: JsonElement) = 

    match el.ValueKind with

    | JsonValueKind.Number ->
        el.GetInt32()

    | JsonValueKind.Array ->
        el.EnumerateArray()
        |> Seq.sumBy sumJson

    | JsonValueKind.Object ->
        
        let hasRed = 
            el.EnumerateObject()
            |> Seq.exists(fun p ->
                p.Value.ValueKind = JsonValueKind.String
                    && p.Value.GetString() = "red")

        if hasRed then
            0
        else
            el.EnumerateObject()
            |> Seq.sumBy (fun p -> sumJson p.Value)

    | _ -> 0


let doc = JsonDocument.Parse json

let root = doc.RootElement

let partTwo = sumJson root

