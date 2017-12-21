let myInput = "oundnydw"

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


let oneRound (xs: int[]) (lengths: int list) (initialOffset: int) (initialSkip: int) : (int * int) =
    List.fold
      (fun (offset, skip) len ->
       let a = offset
       let b = a+len-1
       reverse xs a b
       ((a+len+skip) % xs.Length), skip+1) (initialOffset, initialSkip) lengths


let knothash (input: string) : int[] =
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
    d xs

let popcount (n: int) : int =
    let mutable n = n
    let mutable count = 0
    for i = 1 to 8 do
        count <- count + (n &&& 1)
        n <- n >>> 1
    done
    count

let part1 (input: string) : int =
    {0..127}
    |> Seq.fold (fun count n ->
                 let row = input + "-" + string n
                 let h = knothash row
                 count + (h |> Array.map popcount |> Array.sum)) 0

printfn "%d" (part1 myInput)



type Graph = Map<int, int list>

let spanningTree (startNode: int) (g: Graph) : Graph =
    let rec loop (stack: int list) (acc: Graph) : Graph =
        match stack with
            | [] -> acc
            | x::rest ->
                match Map.tryFind x acc with
                    | None ->
                        let neighbors = Map.find x g
                        loop (neighbors @ rest) (Map.add x neighbors acc)
                    | Some _ ->
                        loop rest acc
    loop [startNode] Map.empty


let singleKey m =
    m |> Map.toSeq |> Seq.head |> fst

let connectedComponents (g: Graph) : int =
    let rec loop (g: Graph) (acc: int) : int =
        if Map.isEmpty g then
            acc
        else
            let st = spanningTree (singleKey g) g
            let g' = Map.fold (fun g k _ -> Map.remove k g) g st
            loop g' (acc + 1)
    loop g 0

let hashToBits (xs: int[]) : bool[] =
    xs
    |> Array.map (fun x ->
                  [| x &&& 0b10000000 <> 0;
                     x &&& 0b01000000 <> 0;
                     x &&& 0b00100000 <> 0;
                     x &&& 0b00010000 <> 0;
                     x &&& 0b00001000 <> 0;
                     x &&& 0b00000100 <> 0;
                     x &&& 0b00000010 <> 0;
                     x &&& 0b00000001 <> 0 |])
    |> Array.concat

let makeGraph (disk: bool[][]) : Graph =
    let get x y =
        if x < 0 || x > 127 || y < 0 || y > 127 then
            false
        else
            disk.[x].[y]
    let num x y =
        x*128+y

    let mutable g = Map.empty
    for x = 0 to 127 do
        for y = 0 to 127 do
            if get x y then
                let neighbors = [
                    (if get (x-1) y then num (x-1) y else -1);
                    (if get (x+1) y then num (x+1) y else -1);
                    (if get x (y-1) then num x (y-1) else -1);
                    (if get x (y+1) then num x (y+1) else -1)
                ]
                let neighbors = List.filter ((<>) -1) neighbors
                g <- Map.add (num x y) neighbors g
        done
    done
    g

let part2 (input: string) : int =
    [|0 .. 127|]
    |> Array.map (fun n -> input + "-" + string n)
    |> Array.map knothash
    |> Array.map hashToBits
    |> makeGraph
    |> connectedComponents

printfn "%d" (part2 myInput)
