let myInput1 = [83;0;193;1;254;237;187;40;88;27;2;255;149;29;42;100]
let myInput2 = "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100"

let reverse (xs: int[]) (i: int) (j: int) =
    let swap x y =
        let t = xs.[x]
        xs.[x] <- xs.[y]
        xs.[y] <- t

    let mutable i = i
    let mutable j = j
    while i < j do
        swap (i % xs.Length) (j % xs.Length)
        i <- i+1
        j <- j-1
    done


let oneRound
    (xs: int[])
    (lengths: int list)
    (initialOffset: int)
    (initialSkip: int) : (int * int) =
    List.fold
      (fun (offset, skip) len ->
       let a = offset
       let b = a+len-1
       reverse xs a b
       ((a+len+skip) % xs.Length), skip+1) (initialOffset, initialSkip) lengths

let part1 (xs: int[]) (lengths: int list) : int =
    oneRound xs lengths 0 0 |> ignore
    xs.[0] * xs.[1]

// let part1Test () =
//     let xs = [|0..4|]
//     let lengths = [3; 4; 1; 5]
//     printfn "%A" (part1 xs lengths)
// part1Test ()

printfn "%A" (part1 [|0..255|] myInput1)

let part2 (input: string) : string =
    let d (xs: int[]) : int[] =
        let out = Array.create 16 0
        for i in 0 .. 255 do
            out.[i / 16] <- out.[i/16] ^^^ xs.[i]
        done
        out

    let xs = [|0..255|]
    let lengths = input |> Seq.toArray |> Array.map int
    let nonce = [|17; 31; 73; 47; 23|]
    let lengths = Array.append lengths nonce |> Array.toList
    Seq.fold
        (fun (offset, skip) _ -> oneRound xs lengths offset skip)
        (0, 0)
        [0..63] |> ignore
    let denseHash = d xs
    String.concat "" (Array.map (sprintf "%02x") denseHash)

// let part2Test () =
//     printfn "%A" (part2 "")
//     printfn "%A" (part2 "AoC 2017")
// part2Test ()

printfn "%s" (part2 myInput2)
