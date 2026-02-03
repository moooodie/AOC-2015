let dimensions = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map (fun dimenStr ->
            let nums = dimenStr.Split 'x'
            in struct (int (nums[0]), int (nums[1]), int (nums[2])))

let partOne = 
    dimensions
    |> Array.sumBy(fun struct (l, w, h) ->
        let struct (side1, side2, side3) = l * w, l * h, w * h

        let minSide = min (min side1 side2) side3

        2 * (side1 + side2 + side3) + minSide)

let partTwo = 
    dimensions
    |> Array.sumBy(fun struct (l, w, h) ->
        l * w * h + min (min (2 * (l + h)) (2 * (l + w))) (2 * (w + h)))