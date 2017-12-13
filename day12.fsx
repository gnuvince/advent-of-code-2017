open System.IO
open System.Text.RegularExpressions

type Graph = Map<int, int list>

let parseLine (line: string) : (int * int list) =
    let mainSplit = Regex @"(\d+) <-> (.*)"
    let neighborSplit = Regex @", "
    let m = mainSplit.Match line
    let src = m.Groups.Item 1 |> string |> int
    let dst =
        neighborSplit.Split (m.Groups.Item 2 |> string)
        |> Array.toList
        |> List.map int
    (src, dst)


let parseGraph (filename: string) : Graph =
    filename
    |> File.ReadLines
    |> Seq.map parseLine
    |> Seq.toList
    |> Map.ofList

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

let part1 (filename: string) : int =
    filename
    |> parseGraph
    |> spanningTree 0
    |> fun graph -> graph.Count

"day12.input" |> part1 |> printfn "%A"

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


let part2 (filename: string) : int =
    filename
    |> parseGraph
    |> connectedComponents

"day12.input" |> part2 |> printfn "%A"
