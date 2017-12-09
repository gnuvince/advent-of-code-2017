open System.IO
open System.Text.RegularExpressions

type Env = Map<string, int>

type Op = {
    reg: string;
    op: (int -> int -> int);
    imm: int;
}

type Cond = {
    reg: string;
    cmp: (int -> int -> bool);
    imm: int;
}

type Instr = {
    op: Op;
    condition: Cond;
}

let scannerRegex =
    Regex @"(\w+) (dec|inc) (-?\d+) if (\w+) (==|!=|<|<=|>|>=) (-?\d+)"

let strToIncDec (str: string) : (int -> int -> int) =
    match str with
        | "inc" -> (fun regVal x -> regVal + x)
        | "dec" -> (fun regVal x -> regVal - x)
        | _ -> failwith "invalid inc/dec op"

let strToCmpOp (str: string) : (int -> int -> bool) =
    match str with
        | "==" -> ( = )
        | "!=" -> ( <> )
        | "<"  -> ( < )
        | "<=" -> ( <= )
        | ">"  -> ( > )
        | ">=" -> ( >= )
        | _ -> failwith "invalid cmp op"

let parseLine (line: string) : Instr =
    let m = scannerRegex.Match(line)
    let op = {
        reg = m.Groups.Item 1 |> string;
        op  = m.Groups.Item 2 |> string |> strToIncDec;
        imm = m.Groups.Item 3 |> string |> int
    }
    let cond = {
        reg = m.Groups.Item 4 |> string;
        cmp = m.Groups.Item 5 |> string |> strToCmpOp;
        imm = m.Groups.Item 6 |> string |> int
    }
    { op = op; condition = cond }

let eval (env: Env) (instr: Instr) : Env =
    let readReg reg =
        match Map.tryFind reg env with
            | None -> 0
            | Some n -> n

    let condRegVal = readReg instr.condition.reg
    if instr.condition.cmp condRegVal instr.condition.imm then
        let opRegVal = readReg instr.op.reg
        env
        |> Map.remove instr.op.reg
        |> Map.add instr.op.reg (instr.op.op opRegVal instr.op.imm)
    else
        env

let part1 (filename: string) : int =
    filename
    |> File.ReadLines
    |> Seq.fold (fun env line -> eval env (parseLine line)) Map.empty
    |> Map.toSeq
    |> Seq.maxBy snd
    |> snd

// "day8.sample" |> part1 |> printfn "%A"
"day8.input"  |> part1 |> printfn "%A"



let part2 (filename: string) : int =
    filename
    |> File.ReadLines
    |> Seq.fold (fun (maxVal, env) line ->
                 let env = eval env (parseLine line)
                 let envMax =
                    if Map.isEmpty env then
                        0
                    else
                        env
                        |> Map.toSeq
                        |> Seq.maxBy snd
                        |> snd
                 (max maxVal envMax, env)) (0, Map.empty)
    |> fst

// "day8.sample" |> part2 |> printfn "%A"
"day8.input" |> part2 |> printfn "%A"
