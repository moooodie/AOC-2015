type Signal = 
    | Value of uint16
    | Wire of string
    | NOT of Signal
    | AND of Signal * Signal
    | OR of Signal * Signal
    | LSHIFT of Signal * int
    | RSHIFT of Signal * int

type Instruction = {
    InputSignal : Signal
    OutputWire : string
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

let rec evalSignal (wires: Map<string, uint16>) signal = 
    let eval = evalSignal wires

    match signal with

    | Value n -> Some n

    | Wire w -> Map.tryFind w wires

    | NOT s ->
        eval s |> Option.map (~~~)

    | AND (l, r) ->
        match eval l, eval r with
        | Some a , Some b -> Some (a &&& b)
        | _ -> None 

    | OR (l, r) ->
        match eval l, eval r with
        | Some a , Some b -> Some (a ||| b)
        | _ -> None 
    
    | LSHIFT (s, n) ->
        eval s |> Option.map ( fun v -> v <<< n)

    | RSHIFT (s, n) ->
        eval s |> Option.map (fun v -> v >>> n)


let instructions = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map strToInstruction
    |> Array.toList

let step wires instructions =
    instructions
    |> List.fold (fun (wires, pending) instr ->
        match evalSignal wires instr.InputSignal with

        | Some value ->
            Map.add instr.OutputWire value wires, pending

        | _ -> 
            wires, instr :: pending
    ) (wires, [])

let rec resolve wires instructions = 
    let wires', pending = step wires instructions

    if List.isEmpty pending then
        wires'
    
    elif pending.Length = instructions.Length then
        failwith "Unresolvable dependency cycle"

    else
        resolve wires' pending

let wires1 = resolve Map.empty instructions

let partOne = wires1["a"]

let instructions2 = 
    instructions
    |> List.filter (fun i -> i.OutputWire <> "b")


let wires2 = 
    resolve (Map.ofList [ ("b", partOne) ]) instructions2

let partTwo = wires2["a"]