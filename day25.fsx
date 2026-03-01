let convStrToInt str = 
    try
        Some (int str)
    with
        | _ -> None

let row, col = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.collect (fun line -> line.Split [|' '; ','; '.' |])
    |> Array.choose convStrToInt
    |> fun arr -> arr[0], arr[1]


let solver targetRow targetCol = 

    let rec recurse curRow curCol curCode = 

        if curRow = targetRow && curCol = targetCol then
            curCode

        else
            let newCode = 
                curCode * 252533L % 33554393L

            let newRow, newCol = 
                if curRow = 1 then
                    curCol + 1, 1

                else
                    curRow - 1, curCol + 1

            recurse newRow newCol newCode

    recurse 1 1 20151125L

let partOne = solver row col