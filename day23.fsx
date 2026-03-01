type Computer = {
    RegisterA: uint
    RegisterB: uint
}
let input =
    System.IO.File.ReadAllLines "input.txt"

open System
let (| Hlf | Tpl | Inc | Jmp | Jie | Jio |) (str: string) = 
    let arr =
        str.Split ([| ','; ' ' |], 
            StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.RemoveEmptyEntries)

    if arr[0] = "hlf" then
        Hlf arr[1]

    elif arr[0] = "tpl" then
        Tpl arr[1]
    
    elif arr[0] = "inc" then
        Inc arr[1]
    
    elif arr[0] = "jmp" then
        Jmp (int arr[1])

    elif arr[0] = "jie" then
        Jie (arr[1] ,int arr[2])

    else
        Jio (arr[1], int arr[2])


let rec solver computer instructionPointer = 

    if instructionPointer < 0 || instructionPointer >= input.Length then
        computer.RegisterB

    else
        match input[instructionPointer] with

        | Hlf reg -> 
            if reg = "a" then
                solver { computer with RegisterA = computer.RegisterA / 2u } 
                    (instructionPointer + 1)

            else 
                solver { computer with RegisterB = computer.RegisterB / 2u }
                    (instructionPointer + 1)

        | Tpl reg ->
            if reg = "a" then
                solver { computer with RegisterA = computer.RegisterA * 3u } 
                    (instructionPointer + 1)

            else 
                solver { computer with RegisterB = computer.RegisterB * 3u }
                    (instructionPointer + 1)


        | Inc reg ->
            if reg = "a" then
                solver { computer with RegisterA = computer.RegisterA + 1u } 
                    (instructionPointer + 1)

            else 
                solver { computer with RegisterB = computer.RegisterB + 1u }
                    (instructionPointer + 1)

        | Jmp offset ->
            solver computer (instructionPointer + offset)

        | Jie (reg, offset) ->
            if reg = "a" && computer.RegisterA % 2u = 0u 
                || reg = "b" && computer.RegisterB % 2u = 0u then
                solver computer (instructionPointer + offset)

            else 
                solver computer (instructionPointer + 1)

        | Jio (reg, offset) ->
            if reg = "a" && computer.RegisterA = 1u 
                || reg = "b" && computer.RegisterB = 1u then
                solver computer (instructionPointer + offset)

            else 
                solver computer (instructionPointer + 1)

let partOne = solver { RegisterA = 0u; RegisterB = 0u } 0

let partTwo = solver { RegisterA = 1u; RegisterB = 0u } 0