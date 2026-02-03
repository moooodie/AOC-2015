open System
open System.Text
open System.Security.Cryptography


let input = 
    let baz = System.IO.File.ReadAllLines "input.txt" in baz[0]

let partOne = 
    Seq.initInfinite id
    |> Seq.find( fun num ->
        let hashedData = 
            MD5.HashData(Encoding.UTF8.GetBytes(String.Concat(input, num)))

        hashedData.Length >= 3 && hashedData[0] = 0uy &&
            hashedData[1] = 0uy && hashedData[2] < 16uy )

let partTwo = 
    Seq.initInfinite id
    |> Seq.find(fun num ->
        let hashedData = 
            MD5.HashData(Encoding.UTF8.GetBytes(String.Concat(input, num)))

        hashedData.Length >= 3 && hashedData[0] = 0uy &&
            hashedData[1] = 0uy && hashedData[2] = 0uy )