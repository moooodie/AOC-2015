open System.Collections.Generic

let input = System.IO.File.ReadAllLines "input.txt"

let isNice (str: string) = 

    let inline isVowel letter = 
        letter = 'a' || letter = 'e' ||
            letter = 'i' || letter = 'o' ||
            letter = 'u'

    let inline isInvalid firstLetter secondLetter = 
        (firstLetter = 'a' || firstLetter = 'c' ||
        firstLetter = 'p' || firstLetter = 'x') &&
            secondLetter = firstLetter + char 1

    let mutable prevLetter = str[0]
    let mutable vowelCount = if isVowel prevLetter then 1 else 0
    let mutable hasDuplicatePair = false
    let mutable hasInvalidStrings = false

    for i in 1..(str.Length - 1) do
        if isVowel (str[i]) then vowelCount <- vowelCount + 1

        if prevLetter = str[i] then hasDuplicatePair <- true

        if isInvalid prevLetter (str[i]) then
            hasInvalidStrings <- true

        prevLetter <- str[i]

    not hasInvalidStrings && vowelCount > 2 && hasDuplicatePair

let isNiceNew (str: string) = 
    let mutable hasRepeatingPairs = false
    let dict = Dictionary<(char * char), int>()
    let mutable index = 0

    while index < str.Length - 1 && not hasRepeatingPairs do
        match dict.TryGetValue((str[index], str[index + 1])) with

        | true, num when num <> index ->
            hasRepeatingPairs <- true
        | true, _ -> ()
        | _ ->
            dict[(str[index], str[index + 1])] <- index + 1

        index <- index + 1

    let mutable hasSingleLetterGap = false
    let mutable index = 2

    while index < str.Length && not hasSingleLetterGap do
        if str[index] = str[index - 2] then
            hasSingleLetterGap <- true

        index <- index + 1

    hasRepeatingPairs && hasSingleLetterGap

let partOne = 
    input
    |> Seq.filter isNice
    |> Seq.length

let partTwo = 
    input
    |> Seq.filter isNiceNew
    |> Seq.length