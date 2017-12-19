open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Env = Map<char, int64>
type Op = Reg of char | Imm of int64

let fetch (env: Env) (r: char) : int64 =
    match Map.tryFind r env with
        | None -> 0L
        | Some n -> n

let valueOf (env: Env) (op: Op) : int64 =
    match op with
        | Imm n -> n
        | Reg r -> fetch env r

let update (env: Env) (op : int64 -> int64 -> int64) (reg: char) (n: int64) : Env =
    let x = fetch env reg
    env |> Map.add reg (op x n)

type Instr =
    | Snd of Op
    | Set of Op * Op
    | Add of Op * Op
    | Mul of Op * Op
    | Mod of Op * Op
    | Rcv of Op
    | Jgz of Op * Op

type Vm = {
    program : Instr array
    pc : int
    env : Env
    played : int64 list
}

let parseOp (op: string) : Op =
    try
        Imm (op |> int64)
    with
        :? System.FormatException -> Reg (op |> char)


let parseInstr (line: string) : Instr =
    match line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) with
        | [|"snd"; op|] -> Snd (parseOp op)
        | [|"rcv"; op|] -> Rcv (parseOp op)
        | [|"set"; o1; o2|] -> Set (parseOp o1, parseOp o2)
        | [|"add"; o1; o2|] -> Add (parseOp o1, parseOp o2)
        | [|"mul"; o1; o2|] -> Mul (parseOp o1, parseOp o2)
        | [|"mod"; o1; o2|] -> Mod (parseOp o1, parseOp o2)
        | [|"jgz"; o1; o2|] -> Jgz (parseOp o1, parseOp o2)


let parse (filename: string) : Instr array =
    filename
    |> File.ReadLines
    |> Seq.map parseInstr
    |> Seq.toArray

exception Done of int64

let step1 (vm: Vm) : Vm =
    if vm.pc >= vm.program.Length then
        vm
    else
        match vm.program.[vm.pc] with
            | Snd op ->
                { vm with played = (valueOf vm.env op) :: vm.played; pc = vm.pc + 1 }
            | Rcv op ->
                if valueOf vm.env op = 0L then
                    { vm with pc = vm.pc + 1 }
                else
                    match vm.played with
                        | [] -> failwith "empty playlist"
                        | h::_ -> raise (Done h)
            | Set (Reg r, o2) ->
                { vm with env = update vm.env (fun _ x -> x) r (valueOf vm.env o2); pc = vm.pc + 1 }
            | Add (Reg r, o2) ->
                { vm with env = update vm.env ( + ) r (valueOf vm.env o2); pc = vm.pc + 1 }
            | Mul (Reg r, o2) ->
                { vm with env = update vm.env ( * ) r (valueOf vm.env o2); pc = vm.pc + 1 }
            | Mod (Reg r, o2) ->
                { vm with env = update vm.env ( % ) r (valueOf vm.env o2); pc = vm.pc + 1 }
            | Jgz (o1, o2) ->
                if valueOf vm.env o1 > 0L then
                    { vm with pc = vm.pc + int (valueOf vm.env o2) }
                else
                    { vm with pc = vm.pc + 1 }


let part1 (filename: string) : int64 =
    let rec loop vm =
        try
            loop (step1 vm)
        with
            | Done x -> x

    let program = filename |> parse
    let vm = {
        program = program
        env = Map.empty
        pc = 0
        played = []
    }
    loop vm

"day18.example" |> part1 |> printfn "%d"
"day18.input" |> part1 |> printfn "%d"



type Vm2 = {
    program : Instr array
    pc : int
    env : Env
    queue : Queue<int64>
    sends : int
}

let finished (vm: Vm2) =
    vm.pc >= vm.program.Length ||
        match vm.program.[vm.pc] with
            | Rcv _ -> vm.queue.Count = 0
            | _ -> false

exception Done2 of int

let part2 (filename: string) : int =
    let single (thisVm: Vm2) (otherVm: Vm2) : Vm2 =
        if finished thisVm then
            thisVm
        else
            match thisVm.program.[thisVm.pc] with
                | Set (Reg r, o2) ->
                    { thisVm with env = update thisVm.env (fun _ x -> x) r (valueOf thisVm.env o2); pc = thisVm.pc + 1 }
                | Add (Reg r, o2) ->
                    { thisVm with env = update thisVm.env ( + ) r (valueOf thisVm.env o2); pc = thisVm.pc + 1 }
                | Mul (Reg r, o2) ->
                    { thisVm with env = update thisVm.env ( * ) r (valueOf thisVm.env o2); pc = thisVm.pc + 1 }
                | Mod (Reg r, o2) ->
                    { thisVm with env = update thisVm.env ( % ) r (valueOf thisVm.env o2); pc = thisVm.pc + 1 }
                | Jgz (o1, o2) ->
                    if valueOf thisVm.env o1 > 0L then
                        { thisVm with pc = thisVm.pc + int (valueOf thisVm.env o2) }
                    else
                        { thisVm with pc = thisVm.pc + 1 }
                | Snd op ->
                    otherVm.queue.Enqueue (valueOf thisVm.env op)
                    { thisVm with pc = thisVm.pc + 1; sends = thisVm.sends + 1 }
                | Rcv (Reg r) ->
                    if thisVm.queue.Count = 0 then
                        thisVm
                    else
                        let x = thisVm.queue.Dequeue()
                        { thisVm with env = Map.add r x thisVm.env; pc = thisVm.pc + 1 }

    let rec loop vm0 vm1 =
        if finished vm0 && finished vm1 then
            raise (Done2 vm1.sends)
        else
            loop (single vm0 vm1) (single vm1 vm0)

    let program = filename |> parse
    let vm0 = { program = program; env = Map.ofList [('p', 0L)]; pc = 0; sends = 0; queue = new Queue<int64>() }
    let vm1 = { program = program; env = Map.ofList [('p', 1L)]; pc = 0; sends = 0; queue = new Queue<int64>() }
    try
        loop vm0 vm1
        0
    with
        Done2 x -> x

"day18.input" |> part2 |> printfn "%d"
