let next (b: uint64) (m: uint64) : uint64 =
    (b * m) % 2147483647UL

let part1 (a: uint64) (b: uint64) (iters: int) : int =
    seq [1 .. iters]
    |> Seq.fold
        (fun (a, b, total) _ ->
         let a' = next a 16807UL
         let b' = next b 48271UL
         let total' = if (a' &&& 0xFFFFUL) = (b' &&& 0xFFFFUL) then total + 1 else total
         (a', b', total')) (a, b, 0)
    |> fun (_, _, total) -> total

//part1 65UL 8921UL 5 |> printfn "%A"
part1 883UL 879UL 40000000 |> printfn "%A"

let rec next2 (b: uint64) (m: uint64) (d: uint64) : uint64 =
    let y = next b m
    if y % d = 0UL then y else next2 y m d


let part2 (a: uint64) (ad: uint64) (b: uint64) (bd: uint64) (iters: int) : int =
    seq [1 .. iters]
    |> Seq.fold
        (fun (a, b, total) _ ->
         let a' = next2 a 16807UL ad
         let b' = next2 b 48271UL bd
         let total' = if (a' &&& 0xFFFFUL) = (b' &&& 0xFFFFUL) then total + 1 else total
         (a', b', total')) (a, b, 0)
    |> fun (_, _, total) -> total


//part2 65UL 4UL 8921UL 8UL 1056 |> printfn "%A"
part2 883UL 4UL 879UL 8UL 5000000 |> printfn "%A"
