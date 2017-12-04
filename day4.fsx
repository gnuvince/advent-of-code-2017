open System.IO

let hasDupes (words : string[]) : bool =
    let counts = Seq.fold (fun s word -> Set.add word s) Set.empty words
    counts.Count <> words.Length

let part1 inputFile =
    File.ReadLines inputFile
    |> Seq.map (fun line -> line.Split [|' '|])
    |> Seq.filter (fun words -> not (hasDupes words))
    |> Seq.length

printfn "%A" (part1 "day4.input")



let areAnagrams (word1: string) (word2: string) : bool =
    word1.Length = word2.Length &&
    begin
        let s1 = Seq.toArray word1 |> Seq.sort
        let s2 = Seq.toArray word2 |> Seq.sort
        Seq.zip s1 s2
        |> Seq.forall (fun (c1, c2) -> c1 = c2)
    end

let hasAnagrams (words: string[]) : bool =
    let mutable foundAnagram = false
    for i = 0 to words.Length - 1 do
        for j = 0 to words.Length - 1 do
            if i <> j then
                foundAnagram <- foundAnagram || areAnagrams words.[i] words.[j]
        done
    done
    foundAnagram


let part2 inputFile =
    File.ReadLines inputFile
    |> Seq.map (fun line -> line.Split [|' '|])
    |> Seq.filter (fun words -> not (hasDupes words) && not (hasAnagrams words))
    |> Seq.length


printfn "%A" (part2 "day4.input")
