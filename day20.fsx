open System.IO
open System.Text.RegularExpressions

let parseTriple (s: string) : (int64 * int64 * int64) =
    let r = Regex @"(-?\d+),(-?\d+),(-?\d+)"
    let m = r.Match s
    (m.Groups.Item 1 |> string |> int64,
     m.Groups.Item 2 |> string |> int64,
     m.Groups.Item 3 |> string |> int64)

type Particle = {
    pos: int64 * int64 * int64
    vel: int64 * int64 * int64
    acc: int64 * int64 * int64
}

let parseLine (s: string) : Particle =
    let r = Regex @"p=<(.*?)>, v=<(.*?)>, a=<(.*?)>"
    let m = r.Match s
    {
        pos = m.Groups.Item 1 |> string |> parseTriple
        vel = m.Groups.Item 2 |> string |> parseTriple
        acc = m.Groups.Item 3 |> string |> parseTriple
    }

let manhattan ((x, y, z) : int64 * int64 * int64) : int64 =
    abs x + abs y + abs z

let part1 (filename: string) : int =
    filename
    |> File.ReadLines
    |> Seq.mapi (fun i line -> (i, parseLine line))
    |> Seq.minBy (fun (_, particle) -> manhattan particle.acc)
    |> fst

"day20.input" |> part1 |> printfn "%d"

let rec dropWhile p l =
    match l with
        | [] -> []
        | h::t -> if p h then dropWhile p t else l

let removeDupes (p: 'a -> 'b) (xs: 'a list) : 'a list =
    let rec loop xs =
        match xs with
            | [] -> []
            | [x] -> [x]
            | a::b::rest ->
                let pa = p a
                let pb = p b
                if pa = pb then
                    loop (dropWhile (fun c -> p c = pa) rest)
                else
                    a :: loop (b :: rest)
    loop (List.sort xs)

let vecAdd (x, y, z) (dx, dy, dz) = (x+dx, y+dy, z+dz)

let step (particles: Particle list) : Particle list =
    particles
    |> List.map (fun p ->
                 let vel' = vecAdd p.vel p.acc
                 {pos = vecAdd p.pos vel'
                  vel = vel'
                  acc = p.acc})


let simulate (rounds: int) (particles: Particle list) : Particle list =
    {1 .. rounds}
    |> Seq.fold (fun ps _ -> ps |> step |> removeDupes (fun p -> p.pos)) particles

let part2 (filename: string) : int =
    filename
    |> File.ReadLines
    |> Seq.map parseLine
    |> Seq.toList
    |> simulate 10000
    |> List.length

"day20.input" |> part2 |> printfn "%d"
