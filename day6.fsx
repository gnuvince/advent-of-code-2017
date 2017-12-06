let myInput = [|0; 5; 10; 0; 11; 14; 13; 4; 11; 8; 8; 7; 1; 4; 12; 11|]

// Return the index of the maximum element of the array.
let argmax (arr: int[]) : int =
    let rec loop i currMax currMaxIdx =
        if i >= arr.Length then
            currMaxIdx
        else
            if arr.[i] > currMax then
                loop (i + 1) arr.[i] i
            else
                loop (i + 1) currMax currMaxIdx
    loop 0 -1 -1

let redistribute (arr: int[]) =
    let rec loop i n =
        if n > 0 then
            arr.[i] <- arr.[i] + 1
            loop ((i + 1) % arr.Length) (n - 1)

    let i = argmax arr
    let n = arr.[i]
    arr.[i] <- 0
    loop ((i + 1) % arr.Length) n


let part1 (arr: int[]) : int =
    let rec loop set =
        redistribute arr
        if Set.contains arr set then
            Set.count set
        else
            loop (Set.add (Array.copy arr) set)
    loop (Set.singleton (Array.copy arr))

let testInput = [|0; 2; 7; 0|]

printfn "%A" (part1 myInput)

let part2 (arr: int[]) : int =
    let rec loop map iter =
        redistribute arr
        if Map.containsKey arr map then
            iter - Map.find arr map
        else
            loop (Map.add (Array.copy arr) iter map) (iter + 1)
    let m = Map.empty |> Map.add (Array.copy arr) 0
    loop m 1

printfn "%A" (part2 myInput)
