(*
[0] : { 0 at 0 }
[0, 1] : { 0 at 0, 1 at 1 }
[0, 2, 1] : { 0 at 0, 1 at 1, 2 at 1 }
[0, 2, 3, 1] : { 0 at 0, 1 at 1, 2 at 1, 3 at 2 }

[-1, -1, -1, -1]
Insert 3 at 2

[-1, -1, 3, -1]
Insert 2 at 1

[-1, 2, 3, -1]
Insert 1 at 1
  Oops, already occupied
Insert 1 at (1+1)
  Oops, already occupied
Insert 1 at (1+1+1)

[-1, 2, 3, 1]
Insert 0 at 0

[0, 2, 3, 1]

*)

let rec insertAt (pos: int) (x: 't) (l: 't list) : 't list =
    match (pos, l) with
        | (0, l) -> x :: l
        | (_, h::t) -> h :: (insertAt (pos - 1) x t)
        | (_, _) -> failwith "out of range"


let spinlock (currPos: int) (x: int) (iters: int) (l: int list) : (int * int list) =
    let nextPos = 1 + (currPos + iters) % x
    (nextPos, insertAt nextPos x l)

let part1 (iters: int) : int =
    let (_, lst) =
        Seq.fold
         (fun (pos, l) x -> spinlock pos x iters l)
         (0, [0])
         (seq [1 .. 2017])
    let idx = 1 + (List.findIndex ((=) 2017) lst)
    List.item idx lst

part1 380 |> printfn "%A"


let part2 (iters: int) : int =
    {1 .. 50000000}
    |> Seq.fold
        (fun (pos, atOne) i ->
         let nextPos = 1 + (pos + iters) % i
         if nextPos = 1 then (nextPos, i) else (nextPos, atOne))
        (0, -1)
    |> snd

part2 380 |> printfn "%A"
