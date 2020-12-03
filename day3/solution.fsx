let lines =
    System.IO.File.ReadLines("input.txt")
    |> Seq.toArray

let getRelativeX absoluteY width slopeX = (absoluteY * slopeX) % width

let getTreesHit (lines: string []) slopeX slopeY =
    let treesHit =
        lines
        |> Array.mapi (fun i x -> x.ToCharArray(), i)
        |> Array.filter (fun (_, idx) -> idx % slopeY = 0)
        |> Array.map (fun (el, _) -> el)
        |> Array.mapi (fun y line ->
            let relativeX = getRelativeX y line.Length slopeX
            let valueAtPosition = line.[relativeX]
            valueAtPosition = '#')
        |> Array.filter (id)
        |> Array.length

    printfn "Right %i, down %i\nTrees Hit: %i\n" slopeX slopeY treesHit
    treesHit

let hit1 = getTreesHit lines 1 1
let hit2 = getTreesHit lines 3 1
let hit3 = getTreesHit lines 5 1
let hit4 = getTreesHit lines 7 1
let hit5 = getTreesHit lines 1 2

printfn
    "Multiplied Result: %A"
    (bigint (hit1)
     * bigint (hit2)
     * bigint (hit3)
     * bigint (hit4)
     * bigint (hit5))
