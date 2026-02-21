let input = 
    let temp = System.IO.File.ReadAllLines "input.txt" in
    int temp[0]


let partOne = 
    Seq.initInfinite id
    |> Seq.map (fun n ->
        n, 
            seq {
                for i in 1 .. (int (sqrt (float n)) ) do
                    if n % i = 0 then 
                        yield i
                        yield n / i
                
                
            })
    |> Seq.pick (fun (n, factors) ->
        let sum = factors |> Seq.sum
        if sum * 10 >= input then
            Some n
        else None)


let partTwo = 
    Seq.initInfinite id
    |> Seq.map (fun n ->
        n, 
            seq {
                for i in 1 .. (int (sqrt (float n)) ) do

                    if n % i = 0 then 
                        if i * 50 >= n then yield i
                        let re = n / i
                        if re * 50 >= n then yield n / i
                          
            })
    |> Seq.pick (fun (n, factors) ->
        let sum = factors |> Seq.sum
        if sum * 11 >= input then
            Some n
        else None)