let input = 
    let baz = System.IO.File.ReadAllLines "input.txt" in baz[0]


let part1 = 
    input
    |> Seq.sumBy(fun sym -> if sym = '(' then 1 else -1 )

let part2 = 
    input
    |> Seq.scan(fun (index, curFloor) sym ->
            index + 1, if sym = '(' then curFloor + 1 else curFloor - 1) (0, 0)
    |> Seq.find(fun (_, curFloor) -> curFloor = -1)
    |> fst