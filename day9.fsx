open System.IO

type State = Group | Garbage

let rec parse (state: State) (depth: int) (total: int) (s: char list) : int =
    match state with
        | Group ->
            match s with
                | [] -> total
                | '{' :: rest -> parse Group (depth + 1) total rest
                | '}' :: rest -> parse Group (depth - 1) (depth + total) rest
                | ',' :: rest -> parse Group depth total rest
                | '<' :: rest -> parse Garbage depth total rest
                | _ -> failwith "unknown char in group state"
        | Garbage ->
            match s with
                | [] -> total
                | '>' :: rest -> parse Group depth total rest
                | '!' :: _ :: rest -> parse Garbage depth total rest
                | _ :: rest -> parse Garbage depth total rest




// // part 1 unit tests
// let part1Test inputStr expected =
//     let actual = parse Group 0 0 (Seq.toList inputStr)
//     (actual = expected, actual, expected)

// printfn "%A" (part1Test "{}" 1)
// printfn "%A" (part1Test "{{{}}}" 6)
// printfn "%A" (part1Test "{{},{}}" 5)
// printfn "%A" (part1Test "{{{},{},{{}}}}" 16)
// printfn "%A" (part1Test "{<a>,<a>,<a>,<a>}" 1)
// printfn "%A" (part1Test "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9)
// printfn "%A" (part1Test "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9)
// printfn "%A" (part1Test "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3)

let part1 (filename: string) : int =
    filename
    |> File.ReadLines
    |> Seq.head
    |> Seq.toList
    |> parse Group 0 0

"day9.input" |> part1 |> printfn "%A"




let rec cleanGarbage (state: State) (total: int) (s: char list) : int =
    match state with
        | Group ->
            match s with
                | [] -> total
                | '{' :: rest -> cleanGarbage Group total rest
                | '}' :: rest -> cleanGarbage Group total rest
                | ',' :: rest -> cleanGarbage Group total rest
                | '<' :: rest -> cleanGarbage Garbage total rest
                | _ -> failwith "unknown char in group state"
        | Garbage ->
            match s with
                | [] -> total
                | '>' :: rest -> cleanGarbage Group total rest
                | '!' :: _ :: rest -> cleanGarbage Garbage total rest
                | _ :: rest -> cleanGarbage Garbage (total+1) rest


// let part2Test inputStr expected =
//     let actual = cleanGarbage Group 0 (Seq.toList inputStr)
//     (actual = expected, actual, expected)

// printfn "%A" (part2Test "{<>}" 0)
// printfn "%A" (part2Test "{<random characters>}" 17)
// printfn "%A" (part2Test "{<<<<>}" 3)
// printfn "%A" (part2Test "{<{!>}>}" 2)
// printfn "%A" (part2Test "{<!!>}" 0)
// printfn "%A" (part2Test "{<!!!>>}" 0)
// printfn "%A" (part2Test "{<{o\"i!a,<{i<a>}" 10)



let part2 (filename: string) : int =
    filename
    |> File.ReadLines
    |> Seq.head
    |> Seq.toList
    |> cleanGarbage Group 0

"day9.input" |> part2 |> printfn "%A"
