open System.IO

let execute (subtractThreshold: int option) (jumps: int[]) : int =
    let rec loop index currIter =
        if index < 0 || index >= jumps.Length then
            currIter
        else
            let nextIndex = index + jumps.[index]
            match subtractThreshold with
                | Some(n) when jumps.[index] >= n ->
                    jumps.[index] <- jumps.[index] - 1
                | _ ->
                    jumps.[index] <- jumps.[index] + 1
            loop nextIndex (currIter + 1)
    loop 0 0

let part1 filename =
    filename
    |> File.ReadLines
    |> Seq.map int
    |> Seq.toArray
    |> execute None

printfn "%A" (part1 "day5.input")

let part2 filename =
    filename
    |> File.ReadLines
    |> Seq.map int
    |> Seq.toArray
    |> execute (Some 3)

printfn "%A" (part2 "day5.input")
