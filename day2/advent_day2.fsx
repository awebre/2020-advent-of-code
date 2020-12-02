type SledRule = { Max: int; Min: int; Character: char }

type TobogganRule =
    { Position1: int
      Position2: int
      Character: char }

let isSledPassValid (rule: SledRule, str) =
    let occurrences =
        str
        |> Seq.filter (fun x -> x = rule.Character)
        |> Seq.length

    match occurrences with
    | x when rule.Min <= x && x <= rule.Max -> true
    | _ -> false

let isTobogganPassValid (rule, str) =
    let charArray = str |> Seq.toArray

    let pos1Hit =
        charArray.[rule.Position1 - 1] = rule.Character

    let pos2Hit =
        charArray.[rule.Position2 - 1] = rule.Character

    match (pos1Hit, pos2Hit) with
    | (true, false) -> true
    | (false, true) -> true
    | (_, _) -> false

let readLines =
    System.IO.File.ReadLines("input_day2.txt")

let nums =
    readLines
    |> Seq.map (fun x ->
        let split = x.Split ':'
        match split.Length with
        | 2 -> Some split
        | _ -> None)
    |> Seq.where (fun x -> x.IsSome)
    |> Seq.map (fun x -> x.Value)
    |> Seq.toList

let pairs =
    nums
    |> List.map (fun x ->
        let unparsedRule = x.[0]
        let parts1 = unparsedRule.Split ' '
        let minAndMax = parts1.[0].Split '-'

        let rule =
            { Min = minAndMax.[0] |> int
              Max = minAndMax.[1] |> int
              Character = parts1.[1] |> char }

        let password = x.[1].Trim()
        (rule, password))

let sledRuleValidCount =
    pairs
    |> List.filter (isSledPassValid)
    |> List.length

printfn "Sled Rule Valid: %i" sledRuleValidCount

let tobogganRuleValid =
    pairs
    |> List.map (fun (sledRule, pass) ->
        ({ Position1 = sledRule.Min
           Position2 = sledRule.Max
           Character = sledRule.Character },
         pass))
    |> List.filter (isTobogganPassValid)

printfn "Toboggan Rule Valid:\n %i" tobogganRuleValid.Length
