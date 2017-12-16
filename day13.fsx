open System
open System.IO

type Firewall = int * int * int // range, pos, step (-1 or +1)

let parse (filename: string) : Map<int, Firewall> =
    filename
    |> File.ReadLines
    |> Seq.map (fun line -> line.Split([|':'|], StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun [|d;r|] -> (int d, int r))
    |> Seq.fold (fun m (d,r) -> Map.add d (r, 0, -1) m) Map.empty

let step (firewalls: Map<int, Firewall>) : Map<int, Firewall> =
    Map.map
        (fun _ (range, pos, step) ->
         let step = if pos = 0 || pos = range-1 then -step else step
         (range, pos + step, step)) firewalls

let severity (firewalls: Map<int, Firewall>) : int =
    let (last, _) = Seq.maxBy fst (Map.toSeq firewalls)
    Seq.fold
        (fun (sev, m) d ->
         match Map.tryFind d m with
            | None -> (sev, step m)
            | Some (r, p, _) ->
                let s = if p = 0 then d*r else 0
                (sev+s, step m)) (0, firewalls) [0 .. last] |> fst


let part1 (filename: string) : int =
    let firewalls = filename |> parse
    severity firewalls

"day13.input" |> part1 |> printfn "%A"


let part2 (filename: string) : int =
    let caughtAtBegining firewalls =
        match Map.tryFind 0 firewalls with
            | Some (_, 0, _) -> true
            | _ -> false

    let rec loop delay firewalls =
        if not (caughtAtBegining firewalls) && severity firewalls = 0 then
            delay
        else
            loop (delay + 1) (step firewalls)
    let firewalls = filename |> parse
    loop 0 firewalls

"day13.input" |> part2 |> printfn "%A"
