let input = 
    let temp = System.IO.File.ReadAllLines "input.txt" in
    temp[0]

let inline charToDigit (letter: char) = int (letter - 'a')

open System.Collections.Generic

let isValidPassword (password: int[]) = 

    if Array.contains 8 password || Array.contains 11 password
        || Array.contains 14 password then
        false
    else
        let mutable hasThreeConsLetters = false

        let dict = Dictionary<int, int>()

        for i in 0.. (password.Length - 2) do

            if password[i] = password[i + 1] then
                dict[password[i]] <- i + 1

            
            if i < password.Length - 2 && password[i] + 1 = password[i + 1]
                && password[i + 1] + 1 = password[i + 2] then
                
                hasThreeConsLetters <- true

        dict.Count > 1 && hasThreeConsLetters

let increase (arr: int array) = 
    let mutable shouldAdd = true
    let mutable index = arr.Length - 1

    while shouldAdd && index > 0 do
        let newNum = arr[index] + 1

        arr[index] <- newNum % 26

        if newNum <= 25 then
            shouldAdd <- false
        else
            index <- index - 1

    arr

        
let partOne = 
    input
    |> Seq.map charToDigit
    |> Array.ofSeq
    |> fun arr ->
        let rec infiniteSeq (arr': int array) = seq{
            yield arr'
            yield! infiniteSeq (increase arr')
        }

        infiniteSeq arr

    |> Seq.find isValidPassword
    |> Seq.map (fun num -> 'a' + char num)
    |> Array.ofSeq
    |> System.String


let partTwo = 
    partOne
    |> Seq.map charToDigit
    |> Array.ofSeq
    |> increase
    |> fun arr ->
        let rec infiniteSeq (arr': int array) = seq{
            yield arr'
            yield! infiniteSeq (increase arr')
        }

        infiniteSeq arr

    |> Seq.find isValidPassword
    |> Seq.map (fun num -> 'a' + char num)
    |> Array.ofSeq
    |> System.String