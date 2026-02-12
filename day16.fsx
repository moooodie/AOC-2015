let input = 
    System.IO.File.ReadAllLines "input.txt"

type Sue = {
    Number: int
    Properties: Map<string, int>
}

type MachineSpecifics = 
    | Exact of int
    | NotExact of (int -> bool)

let target = 
    [|
        "children", 3; "cats", 7; "samoyeds", 2; "pomeranians", 3
        "akitas", 0; "vizslas", 0; "goldfish", 5; "trees", 3
        "cars", 2; "perfumes", 1
    |]
    |> Map.ofArray

open System
let sues = 
    input
    |> Array.map (fun str ->
        let strs = 
            str.Split ( [| ' '; ':'; ',' |], 
                StringSplitOptions.RemoveEmptyEntries)
                
        let mappings = 
            seq { for i in 2..2..(strs.Length - 2) -> i }
            |> Seq.map (fun index -> strs[index], int (strs[index + 1]))
            |> Map.ofSeq
            
        {
            Number = int strs[1]
            Properties = mappings
        })

let partOne = 
    sues
    |> Array.find (fun sue ->
        sue.Properties
        |> Map.forall (fun prop amount -> target |> Map.find prop = amount))
    |> fun { Number = n} -> n

let targetTwo = 
    [|
        "children", Exact 3; "samoyeds", Exact 2; "cars", Exact 2

        "cats", NotExact (fun n -> n > 7); "trees", NotExact (fun n -> n > 3) 

        "pomeranians", NotExact (fun n -> n < 3); "goldfish", NotExact (fun n -> n < 5)

        "akitas", Exact 0; "vizslas", Exact 0;  "perfumes", Exact 1
    |]
    |> Map.ofArray


let partTwo = 
    sues
    |> Array.find (fun sue ->
        sue.Properties
        |> Map.forall (fun prop amount ->
            match targetTwo |> Map.find prop with
            | Exact num -> num = amount
            | NotExact f -> f amount))
    |> fun { Number = n } -> n