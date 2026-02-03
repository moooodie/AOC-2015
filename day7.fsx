type Signal = 
    | Value of uint16
    | Wire of string
    | NOT of input: Signal
    | AND of left: Signal * right: Signal
    | OR of left: Signal * right: Signal
    | LSHIFT of left: Signal * value: int
    | RSHIFT of left: Signal * value: int

type Instruction = {
    InputSignal: Signal
    OutputWire: string
}

open System
let strToInstruction (ins: string) : Instruction =
    let parts = 
        ins.Split ("->", 
            StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

    let output = parts[parts.Length - 1]

    let inputSignal = parts[0].Split ' '

    if inputSignal.Length = 1 && Char.IsDigit(inputSignal[0][0]) then
        //it is a number value 
        {
            InputSignal = Value (uint16 (inputSignal[0]))
            OutputWire = output
        }

    elif inputSignal.Length = 1 then
        //it is a wire value
        {
            InputSignal = Wire (inputSignal[0])
            OutputWire = output
        }

    elif inputSignal.Length = 2 && Char.IsDigit(inputSignal[1][0]) then
        //it is NOT gate with number value
        {
            InputSignal = NOT (Value (uint16 (inputSignal[1])))
            OutputWire = output
        }
    
    elif inputSignal.Length = 2 then
        //it is a NOT gate with wire value
        {
            InputSignal = NOT (Wire (inputSignal[1]))
            OutputWire = output
        }

    elif inputSignal[1] = "AND" then
        //AND GATE
        let left, right = inputSignal[0], inputSignal[2]

        let leftVal = 
            if Char.IsDigit(left[0]) then
                Value (uint16 left)
            else Wire left

        let rightVal = 
            if Char.IsDigit(right[0]) then
                Value (uint16 right)
            else Wire right

        {
            InputSignal = AND (leftVal, rightVal)
            OutputWire = output
        }
    elif inputSignal[1] = "OR" then
        //OR GATE
        let left, right = inputSignal[0], inputSignal[2]

        let leftVal = 
            if Char.IsDigit(left[0]) then
                Value (uint16 left)
            else Wire left

        let rightVal = 
            if Char.IsDigit(right[0]) then
                Value (uint16 right)
            else Wire right

        {
            InputSignal = OR (leftVal, rightVal)
            OutputWire = output
        }

    elif inputSignal[1] = "LSHIFT" then
        //LSHIFT GATE
        let left, right = inputSignal[0], inputSignal[2]

        let leftVal = 
            if Char.IsDigit(left[0]) then
                Value (uint16 left)
            else Wire left

        let rightVal = int right

        {
            InputSignal = LSHIFT (leftVal, rightVal)
            OutputWire = output
        }
    else 
        //RSHIFT GATE
        let left, right = inputSignal[0], inputSignal[2]

        let leftVal = 
            if Char.IsDigit(left[0]) then
                Value (uint16 left)
            else Wire left

        let rightVal = int right

        {
            InputSignal = RSHIFT (leftVal, rightVal)
            OutputWire = output
        }


open System.Collections.Generic

let input = 
    System.IO.File.ReadAllLines "input.txt"


let instructionQueue = 
    input
    |> Array.map strToInstruction
    |> Queue

let dict = Dictionary<string, uint16>()


while instructionQueue.Count > 0 do
    let { InputSignal = signal; OutputWire = output } = 
        instructionQueue.Dequeue()

    
    let value = 
        match signal with
        
        | Value num -> Some num

        | Wire str ->
            match dict.TryGetValue str with
            | true, num -> Some num
            | _ -> None

        | NOT (Value num) ->
            Some ~~~ num
        
        | NOT (Wire str) ->
            match dict.TryGetValue str with
            | true, num -> Some ~~~ num
            | _ -> None

        | AND (leftSignal, rightSignal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "AND GATE can only have wire or val on side"

            let rightVal = 
                match rightSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "AND GATE can only have wire or val on side"

            match leftVal, rightVal with
            | Some num1, Some num2 ->
                Some (num1 &&& num2)
            | _ -> None
      
        | OR (leftSignal, rightSignal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "OR GATE can only have wire or val on side"

            let rightVal = 
                match rightSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "OR GATE can only have wire or val on side"

            match leftVal, rightVal with
            | Some num1, Some num2 ->
                Some (num1 ||| num2)
            | _ -> None

        | LSHIFT (leftSignal, shiftVal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "LSHIFT GATE can only have wire or val on side"

            leftVal
            |> Option.map (fun num -> num <<< shiftVal)

        | RSHIFT (leftSignal, shiftVal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "RSHIFT GATE can only have wire or val on side"

            leftVal
            |> Option.map (fun num -> num >>> shiftVal)

        | _ -> failwith "NOT GATE can only have value or wire"

    
    match value with
    | Some num ->
        dict[output] <- num

    | _ -> instructionQueue.Enqueue { InputSignal = signal; OutputWire = output }


let partOne = dict["a"]

let instructionQueueNew = 
    input
    |> Array.map strToInstruction
    |> Array.filter (fun { OutputWire = output } -> output <> "b")
    |> Queue

dict.Clear()
dict["b"] <- partOne


while instructionQueueNew.Count > 0 do
    let { InputSignal = signal; OutputWire = output } = 
        instructionQueueNew.Dequeue()

    
    let value = 
        match signal with
        
        | Value num -> Some num

        | Wire str ->
            match dict.TryGetValue str with
            | true, num -> Some num
            | _ -> None

        | NOT (Value num) ->
            Some ~~~ num
        
        | NOT (Wire str) ->
            match dict.TryGetValue str with
            | true, num -> Some ~~~ num
            | _ -> None

        | AND (leftSignal, rightSignal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "AND GATE can only have wire or val on side"

            let rightVal = 
                match rightSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "AND GATE can only have wire or val on side"

            match leftVal, rightVal with
            | Some num1, Some num2 ->
                Some (num1 &&& num2)
            | _ -> None
      
        | OR (leftSignal, rightSignal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "OR GATE can only have wire or val on side"

            let rightVal = 
                match rightSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "OR GATE can only have wire or val on side"

            match leftVal, rightVal with
            | Some num1, Some num2 ->
                Some (num1 ||| num2)
            | _ -> None

        | LSHIFT (leftSignal, shiftVal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "LSHIFT GATE can only have wire or val on side"

            leftVal
            |> Option.map (fun num -> num <<< shiftVal)

        | RSHIFT (leftSignal, shiftVal) ->

            let leftVal = 
                match leftSignal with
                | Value num -> Some num
                | Wire str ->
                    match dict.TryGetValue str with
                    | true, num -> Some num
                    | _ -> None
                | _ -> failwith "RSHIFT GATE can only have wire or val on side"

            leftVal
            |> Option.map (fun num -> num >>> shiftVal)

        | _ -> failwith "NOT GATE can only have value or wire"

    
    match value with
    | Some num ->
        dict[output] <- num

    | _ -> instructionQueueNew.Enqueue { InputSignal = signal; OutputWire = output }



let partTwo = dict["a"]