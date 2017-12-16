open System.IO
open System.Text.RegularExpressions

let reverse (a: char[]) (i: int) (j: int) =
    let mutable i = i
    let mutable j = j
    while i < j do
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t
        i <- i+1
        j <- j-1
    done

let rotate (a: char[]) (n: int) =
    let last = a.Length - 1
    let p = last - n
    reverse a 0 p
    reverse a (p+1) last
    reverse a 0 last

let swapByPosition (a: char[]) (i: int) (j: int) =
    let t = a.[i]
    a.[i] <- a.[j]
    a.[j] <- t

let swapByName (a: char[]) (x: char) (y: char) =
    let i = Array.findIndex ((=) x) a
    let j = Array.findIndex ((=) y) a
    swapByPosition a i j

type Instr =
    | Spin of int
    | Swap of int * int
    | Pair of char * char


let spinRegex = Regex @"s(\d+)"
let swapRegex = Regex @"x(\d+)/(\d+)"
let pairRegex = Regex @"p([a-p])/([a-p])"

let parseInstruction (s: string) : Instr =
    if s.StartsWith "s" then
        let m = spinRegex.Match(s)
        Spin (m.Groups.Item 1 |> string |> int)
    elif s.StartsWith "x" then
        let m = swapRegex.Match(s)
        let i = m.Groups.Item 1 |> string |> int
        let j = m.Groups.Item 2 |> string |> int
        Swap (i, j)
    elif s.StartsWith "p" then
        let m = pairRegex.Match(s)
        let i = m.Groups.Item 1 |> string |> char
        let j = m.Groups.Item 2 |> string |> char
        Pair (i, j)
    else
        failwith "nomatch"

// "s1" |> parseInstruction |> printfn "%A"
// "s16" |> parseInstruction |> printfn "%A"
// "x1/2" |> parseInstruction |> printfn "%A"
// "x16/15" |> parseInstruction |> printfn "%A"
// "pa/b" |> parseInstruction |> printfn "%A"

let parse (filename: string) : Instr array =
    filename
    |> File.ReadAllText
    |> fun s -> s.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseInstruction

let applyInstr (a: char[]) (instr: Instr) =
    match instr with
        | Spin x -> rotate a x
        | Swap (i, j) -> swapByPosition a i j
        | Pair (x, y) -> swapByName a x y

let applyInstrs (a: char[]) (instrs: Instr array) =
    Array.iter (fun instr -> applyInstr a instr) instrs

let part1 (filename: string) : string =
    let instrs = filename |> parse
    let dancers = [|'a' .. 'p'|]
    applyInstrs dancers instrs
    Array.fold (fun s c -> s + string c) "" dancers

"day16.input" |> part1 |> printfn "%s"


let charArrayToString (a: char[]) : string =
    Array.fold (fun s c -> s + string c) "" a


exception Break

let part2 (filename: string) : unit =
    let instrs = filename |> parse
    let dancers = [|'a' .. 'p'|]
    let seen = System.Collections.Generic.List<string>(1000)
    try
        for i in 0 .. 999999999 do
            let ds = charArrayToString dancers
            if seen.Contains ds then
                let ans = seen.[1000000000 % i]
                printfn "%s" ans
                raise Break
            else
                seen.Add (charArrayToString dancers) |> ignore
                applyInstrs dancers instrs
        done
    with Break ->
        printfn "%A" seen

"day16.input" |> part2
