let readLines =
    System.IO.File.ReadLines("input_day1.txt")

let nums =
    readLines
    |> Seq.map (fun x -> x |> int)
    |> Seq.toList

let pairs =
    nums
    |> List.collect (fun x -> nums |> List.map (fun y -> (x, y)))
    |> List.filter (fun (x, y) -> x + y = 2020)

let triples =
    nums
    |> List.collect (fun x ->
        nums
        |> List.collect (fun y -> nums |> List.map (fun z -> (x, y, z))))
    |> List.filter (fun (x, y, z) -> x + y + z = 2020)

let (pairFirst, pairSecond) = pairs.Head

let (tripleFirst, tripleSecond, tripleThird) = triples.Head

printfn "Pair Result: %d" (pairFirst * pairSecond)

printfn "Triple Result: %d" (tripleFirst * tripleSecond * tripleThird)
