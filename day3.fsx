(*
37 36  35  34  33  32 31
38 17  16  15  14  13 30
39 18   5   4   3  12 29
40 19   6   1   2  11 28
41 20   7   8   9  10 27
42 21  22  23  25  25 26
43 44  45  46  47  48 49
*)


let myInput = 347991


// Returns the 0-indexed layer of a given number.
// E.g.: 1 is layer 0, 2-9 are layer 1, 10-25 layer2, etc.
let layerOf (n: int) : int =
    let rec loop layer =
        if layer*layer >= n then
            layer
        else
            loop (layer + 2)
    (loop 1)/2

// Returns the first and last numbers in a layer.
let rangeOfLayer (layer: int) : (int * int) =
    let n = max 0 (2*layer - 1)
    let m = 2*layer + 1
    let a = n*n + 1
    let b = m*m
    (a, b)

// Returns the x,y coordinates of a number.
// 1 is at 0,0.
// TODO(vfoley): closed-form.
let coordsOf (n: int) : (int * int) =
    let rec loop curr first layer (x, y) (dx, dy) =
        if curr = n then
            (x, y)
        elif curr < first then
            (-1, -1)
        else
            let x' = x + dx
            let y' = y + dy
            if x' >= -layer && x' <= layer && y' >= -layer && y' <= layer then
                loop (curr - 1) first layer (x', y') (dx, dy)
            else
                let (dx', dy') =
                    match (dx, dy) with
                    | (-1, 0) -> (0, 1)
                    | (0, 1) -> (1, 0)
                    | (1, 0) -> (0, -1)
                    | (0, -1) -> (-1, 0)
                loop curr first layer (x, y) (dx', dy')

    let layer = layerOf n
    let (a, b) = rangeOfLayer layer
    let q = (b - a + 1) / 4
    loop b a layer (layer, -layer) (0, -1)

let part1 (n: int) : int =
    let (x, y) = coordsOf n
    abs x + abs y

let sumOfNeighbors x y (mat : int[,]) : int =
    [for dx in [-1 .. 1] do
     for dy in [-1 .. 1] do
     yield mat.[x+dx, y+dy]]
    |> Seq.sum

let part2 (n: int) : int =
    let layer = layerOf n
    let size = 2*(layer + 1) + 1
    let matrix = Array2D.zeroCreate size size
    matrix.[size/2, size/2] <- 1

    let rec loop curr =
        let (x, y) = coordsOf curr
        let (x, y) = (x + size/2, y + size/2)
        let s = sumOfNeighbors x y matrix
        if s > n then
            s
        else
            matrix.[x, y] <- s
            loop (curr + 1)

    loop 1

printfn "%A" (part1 myInput)
printfn "%A" (part2 myInput)
