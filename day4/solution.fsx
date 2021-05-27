open System
open System.Linq
open System.Text.RegularExpressions

type ValidationRule =
    { PropertyType: string
      Rule: string -> bool }

let batch = System.IO.File.ReadAllText("invalid.txt")

let validEyeColors =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]

let tryParseInt s =
    try
        s |> int |> Some
    with :? FormatException -> None

let isADigit (c: char) = Char.IsDigit(c)

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let byrRule value =
    match value |> tryParseInt with
    | Some intVal when 1920 <= intVal && intVal <= 2002 -> true
    | _ -> false

let iyrRule value =
    match value |> tryParseInt with
    | Some intVal when 2010 <= intVal && intVal <= 2020 -> true
    | _ -> false

let eyrRule value =
    match value |> tryParseInt with
    | Some intVal when 2020 <= intVal && intVal <= 2030 -> true
    | _ -> false

let hgtRule (value: string) =
    let valid =
        match value with
        | strVal when strVal.EndsWith("cm") ->
            match strVal.Split("cm").[0] |> tryParseInt with
            | Some cm when 150 <= cm && cm <= 193 -> true
            | _ -> false
        | strVal when strVal.EndsWith("in") ->
            match strVal.Split("in").[0] |> tryParseInt with
            | Some inches when 59 <= inches && inches <= 76 -> true
            | _ -> false
        | _ -> false

    // if not valid then printfn "%s is not a valid height" value
    valid

let hclRule value =
    let valid =
        match value with
        | Regex @"[#][a-z\d]{6}$" _ -> true
        | _ -> false

    // if not valid
    // then printfn "%s is not a valid Hair Color" value
    valid

let pidRule (value: string) =
    match value.ToCharArray() with
    | chars when chars.Length = 9 -> chars |> Array.forall (isADigit)
    | _ -> false

let propertyRules =
    [ { PropertyType = "byr"; Rule = byrRule }
      { PropertyType = "iyr"; Rule = iyrRule }
      { PropertyType = "eyr"; Rule = eyrRule }
      { PropertyType = "hgt"; Rule = hgtRule }
      { PropertyType = "ecl"
        Rule = fun value -> List.contains value validEyeColors }
      { PropertyType = "hcl"; Rule = hclRule }
      { PropertyType = "pid"; Rule = pidRule } ]

let isPassportValid validate (passport: (string * string) seq) =
    let validPassport =
        propertyRules
        |> List.forall (fun required ->
            (passport
             |> Seq.exists (fun (propType, value) ->
                 let isProp = propType = required.PropertyType

                 let isValid = isProp && (not validate || required.Rule value)

                 if validate && isProp && not isValid then printfn "%s is not a valid value for %s" value propType

                 isValid)))

    if not validPassport then printfn "%A is not valid" passport

    validPassport

let passports =
    batch.Split("\n\n")
    |> Seq.map (fun passGroup ->
        passGroup.Split("\n")
        |> Seq.collect (fun y ->
            y.Split(" ")
            |> Seq.map (fun propGroup ->
                let prop = propGroup.Split(":")
                (prop.[0], prop.[1]))))

let validPassPart1 =
    (passports |> Seq.filter (isPassportValid false)).Count()

printfn "Have all required fields: %i" validPassPart1

let validPassPart2 =
    (passports |> Seq.filter (isPassportValid true)).Count()

printfn "Have all required fields and valid data: %i" validPassPart2
