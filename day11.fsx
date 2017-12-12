open System.IO

let dxdydz (dir: string) : (int * int * int) =
    match dir with
        | "n"  -> ( 0, +1, -1)
        | "s"  -> ( 0, -1, +1)
        | "nw" -> (-1, +1,  0)
        | "ne" -> (+1,  0, -1)
        | "sw" -> (-1,  0, +1)
        | "se" -> (+1, -1,  0)

let part1 (filename: string) : int =
    filename
    |> File.ReadAllText
    |> fun s -> s.Trim ()
    |> fun s -> s.Split [|','|]
    |> Array.fold (fun (x,y,z) dir ->
                   let (dx, dy, dz) = dxdydz dir
                   (x+dx, y+dy, z+dz)) (0,0,0)
    |> fun (x,y,z) -> (abs x + abs y + abs z) / 2


part1 "day11.input" |> printfn "%A"


let part2 (filename: string) : int =
    filename
    |> File.ReadAllText
    |> fun s -> s.Trim ()
    |> fun s -> s.Split [|','|]
    |> Array.fold (fun ((x,y,z), acc) dir ->
                   let (dx, dy, dz) = dxdydz dir
                   let coords = (x+dx, y+dy, z+dz)
                   (coords, coords::acc)) ((0,0,0), [])
    |> snd
    |> List.map (fun (x,y,z) -> (abs x + abs y + abs z) / 2)
    |> List.max

part2 "day11.input" |> printfn "%A"
