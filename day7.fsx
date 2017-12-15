open System.IO
open System.Text.RegularExpressions

type entry = {
    name : string;
    weight : int;
    children : string list
}

let parseEntry (line: string) : entry =
    let regex1 = Regex @"(\w+) \((\d+)\)"
    let m = regex1.Match(line)
    let name: string = m.Groups.Item 1 |> string
    let weight = m.Groups.Item 2 |> string |> int
    let children =
        match line.IndexOf "->" with
        | -1 -> []
        |  n ->
            line.[n+2..].Replace(" ", "").Split([|','|])
            |> Seq.toList

    { name=name; weight=weight; children=children }

let parse (filename: string) : entry list =
    filename
    |> File.ReadLines
    |> Seq.map parseEntry
    |> Seq.toList

let rec buildParentLinks acc entries =
    match entries with
        | [] -> acc
        | {name=name; weight=_; children=children} :: rest ->
            let acc = Seq.fold (fun acc child -> Map.add child name acc) acc children
            buildParentLinks acc rest

let findRoot m =
    let rec loop currKey =
        match Map.tryFind currKey m with
            | None -> currKey
            | Some parent -> loop parent

    let key = m |> Map.toSeq |> Seq.head |> fst
    loop key

let part1 (filename: string) : string =
    filename
    |> parse
    |> buildParentLinks Map.empty
    |> findRoot


printfn "%s" (part1 "day7.input")

type tree =
    | Leaf of string * int
    | Node of string * int * tree list

let buildWeightTable entries =
    List.fold (fun m {name=name; weight=weight} -> Map.add name weight m) Map.empty entries

let revLinks m =
    Map.fold (fun m' child parent ->
              match Map.tryFind parent m' with
              | None -> Map.add parent [child] m'
              | Some children ->
                let m'' = Map.remove parent m'
                Map.add parent (child :: children) m'') Map.empty m


let rec buildTree root parentToChildren weights =
    let weight = Map.find root weights
    match Map.tryFind root parentToChildren with
        | None -> Leaf (root, weight)
        | Some children -> Node (root, weight, List.map (fun child -> buildTree child parentToChildren weights) children)

let rec weightTree t =
    match t with
        | Leaf (_, w) -> w
        | Node (_, w, children) ->
            w + (Seq.sumBy weightTree children)

let rec allEqual (ints: int list) : bool =
    match ints with
        | [] -> true
        | [_] -> true
        | x :: y :: rest -> x = y && allEqual (y :: rest)

let treeToString tree =
    match tree with
        | Leaf(name, weight) -> sprintf "%s_%d_%d" name weight (weightTree tree)
        | Node(name, weight, _) -> sprintf "%s_%d_%d" name weight (weightTree tree)

let rec treeToDot tree =
    match tree with
        | Leaf (_, _) -> ""
        | Node (name, weight, children) ->
            let childrenLinks = List.map treeToDot children
            let thisLink = List.map (fun child -> sprintf "%s -> %s;\n" (treeToString tree) (treeToString child)) children
            (childrenLinks @ thisLink) |> List.fold (+) ""

let part2 (filename: string) : string =
    let entries = filename |> parse
    let childToParent = buildParentLinks Map.empty entries
    let root = findRoot childToParent
    let parentToChildren = revLinks childToParent
    let weights = buildWeightTable entries
    let tree = buildTree root parentToChildren weights
    treeToDot tree

"day7.input" |> part2 |> printfn "%A"
