open System
let replace (replacing: string) startOfReplace endofReplace (baseString: string) = 
    let arr = 
        [|
            let mutable index = 0

            while index < startOfReplace do
                yield baseString[index]
                index <- index + 1

            yield! replacing

            index <- endofReplace

            while index < baseString.Length do
                yield baseString[index]
                index <- index + 1
        |]

    String arr

let input = System.IO.File.ReadAllLines "input.txt"

let replacements, medicine = 

    input
    |> Array.splitAt (Array.IndexOf(input, String.Empty))
    |> fun (s1, s2) ->
        let map = 
            s1
            |> Array.map (fun str ->
                let strs = str.Split " => " in
                strs[0], strs[1])
            |> Array.groupBy (fun (input, out) ->
                input)
            
            |> Array.map (fun (input, arr) ->
                input,
                    arr
                    |> Array.map (fun (_, output) -> output))

            |> Map.ofArray


        map, s2[1]
       
let getReplacements str = 
    match replacements |> Map.tryFind str with
    | None -> Array.empty
    | Some n -> n

let allPossibleTransformations (str: string) = 

    seq { 0 .. (str.Length - 1)}
    |> Seq.collect (fun index ->
        seq {
            for repl in 
                getReplacements (str.Substring(index, 1)) do

                yield replace repl index (index + 1) str

            
            if index <> str.Length - 1 then
                for repl in 
                    getReplacements (str.Substring(index, 2)) do

                    yield replace repl index (index + 2) str
        })
    |> Set

let partOne = 
    allPossibleTransformations medicine
    |> Set.count

let reverseReplacements = 
    replacements
    |> Map.toArray
    |> Array.collect (fun (input, output) ->
        
        output
        |> Array.map (fun out ->
            out, input))
    |> Array.sortByDescending (fun (output, _) -> output.Length)
    |> List.ofArray


//slow as balls, not even my solution, but it works.
let partTwo =
    
    let rec solver' (curMolecules: string list) step = 

        if curMolecules |> List.exists(fun str -> 
            replacements["e"] |> Array.contains str) then
            step + 1

        else

            let newMolecules = 
                curMolecules
                |> List.collect (fun mol ->
                    reverseReplacements
                    |> List.choose (fun (output, input) ->
                        let index = mol.IndexOf output
                        if index = -1 then
                            None
                        else
                            Some (replace input index (index + output.Length) mol)
                        ))
                |> List.distinct
                |> List.sortBy (fun str -> str.Length)
                |> List.truncate 1000

            solver' newMolecules (step + 1)

    solver' [ medicine ] 0

//instantaneous appeoach, based on 
//https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/
let noOfElements = 
    medicine
    |> Seq.filter (fun c -> Char.IsAsciiLetterUpper c)
    |> Seq.length


let numberOfRnAr = 
    seq {0 .. medicine.Length - 2}
    |> Seq.filter (fun index ->
        medicine[index] = 'R' && medicine[index + 1] = 'n'
            || medicine[index] = 'A' && medicine[index + 1] = 'r')
    |> Seq.length

let numberOfY = 
    medicine
    |> Seq.filter ((=) 'Y')
    |> Seq.length

let alsoPartTwo = 
    noOfElements - numberOfRnAr - 2 * numberOfY - 1