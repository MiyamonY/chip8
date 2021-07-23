open StdLabels
open Opecode
open Memory
open Util
open State

let unittest = ref false
let use_shift_second_register = false

let fetch pc =
  (memory.(pc) lsl 8) lor memory.(succ pc)

let decode binary_code =
  let split code =
    Array.init 4 ~f:(fun i -> (code lsr (4 * i)) land 0x0f) in
  let rec join = function
    | [] -> 0x00
    | x::l -> x + ((join l) lsl 4)
  in
  match split binary_code with
  | [|0x0; 0xE; 0x0; 0x0|] -> DisplayClear
  | [|0xE; 0xE; 0x0; 0x0|] -> Ret
  | [| n0;  n1;  n2; 0x1|] -> Jmp (join [n0;n1;n2])
  | [| n0;  n1;  n2; 0x2|] -> Call (join [n0;n1;n2])
  | [| n0;  n1;   x; 0x3|] -> SkipEq (x, join [n0;n1])
  | [| n0;  n1;   x; 0x4|] -> SkipNeq (x, join [n0;n1])
  | [|0x0;   y;   x; 0x5|] -> SkipCond (x, y)
  | [| n0;  n1;   x; 0x6|] -> SetI (x, join [n0;n1])
  | [| n0;  n1;   x; 0x7|] -> AddI (x, join [n0;n1])
  | [|0x0;   y;   x; 0x8|] -> Set (x,y)
  | [|0x1;   y;   x; 0x8|] -> Or (x,y)
  | [|0x2;   y;   x; 0x8|] -> And (x,y)
  | [|0x3;   y;   x; 0x8|] -> Xor(x,y)
  | [|0x4;   y;   x; 0x8|] -> Add(x,y)
  | [|0x5;   y;   x; 0x8|] -> Sub(x,y)
  | [|0x6;   y;   x; 0x8|] -> RShift(x,y)
  | [|0x7;   y;   x; 0x8|] -> MSub(x,y)
  | [|0xE;   y;   x; 0x8|] -> LShift(x,y)
  | [|0x0;   y;   x; 0x9|] -> Neq(x,y)
  | [| n0;  n1;  n2; 0xA|] -> SetAddr (join [n0;n1;n2])
  | [| n0;  n1;  n2; 0xB|] -> JmpOffset (join [n0;n1;n2])
  | [| n0;  n1;   x; 0xC|] -> Rand (x, join [n0;n1])
  | [| n0;   y;   x; 0xD|] -> Draw (x, y, n0)
  | [|0xE; 0x9;   x; 0xE|] -> SkipKey x
  | [|0x1; 0xA;   x; 0xE|] -> SkipNKey x
  | [|0x7; 0x0;   x; 0xF|] -> GetTimer x
  | [|0xA; 0x0;   x; 0xF|] -> Key x
  | [|0x5; 0x1;   x; 0xF|] -> SetTimer x
  | [|0x8; 0x1;   x; 0xF|] -> SetSound x
  | [|0xE; 0x1;   x; 0xF|] -> AddInput x
  | [|0x9; 0x2;   x; 0xF|] -> SetFont x
  | [|0x3; 0x3;   x; 0xF|] -> StoreBCD x
  | [|0x5; 0x5;   x; 0xF|] -> Dump x
  | [|0x5; 0x6;   x; 0xF|] -> Load x
  | _ -> failwith @@ Printf.sprintf "invalid binary code 0x%X" binary_code

let execute chip8 code =
  Printf.printf "0x%x: %s\n" chip8.pc @@ print chip8 code;
  match code with
  | DisplayClear ->
    {chip8 with pc = chip8.pc+2; gfx = Array.init (32*64) ~f:(fun _ -> 0)}
  | Ret ->
    let pc = Stack.pop chip8.stack in
    {chip8 with pc = pc}
  | Jmp addr -> {chip8 with pc = addr}
  | Call addr ->
    Stack.push (chip8.pc + 2) chip8.stack;
    {chip8 with pc = addr}
  | SkipEq (reg, v) ->
    let offset = if chip8.registers.(reg) = v then 4 else 2 in
    {chip8 with pc = chip8.pc + offset}
  | SkipNeq (reg, v) ->
    let offset = if chip8.registers.(reg) <> v then 4 else 2 in
    {chip8 with pc = chip8.pc + offset}
  | SkipCond (reg0, reg1) ->
    let offset = if chip8.registers.(reg0) = chip8.registers.(reg1) then 4 else 2 in
    {chip8 with pc = chip8.pc + offset}
  | SetI (reg, v) ->
    begin
      chip8.registers.(reg) <- v;
      {chip8 with pc = chip8.pc+2 }
    end
  | AddI (x, n) ->
    begin
      chip8.registers.(x) <- chip8.registers.(x) + n;
      chip8.registers.(x) <- chip8.registers.(x) mod 0x100;
      {chip8 with pc = chip8.pc+2 }
    end
  | Set (x, y) ->
    begin
      chip8.registers.(x) <- chip8.registers.(y);
      {chip8 with pc = chip8.pc+2 }
    end
  | Or (x, y) ->
    begin
      chip8.registers.(x) <- chip8.registers.(x) lor chip8.registers.(y);
      chip8.registers.(0xf) <- 0;
      {chip8 with pc = chip8.pc+2 }
    end
  | And (x, y) ->
    begin
      chip8.registers.(x) <- chip8.registers.(x) land chip8.registers.(y);
      chip8.registers.(0xf) <- 0;
      {chip8 with pc = chip8.pc+2 }
    end
  | Xor (x, y) ->
    begin
      chip8.registers.(x) <- (chip8.registers.(x) lxor chip8.registers.(y)) land 0xff;
      chip8.registers.(0xf) <- 0;
      {chip8 with pc = chip8.pc+2 }
    end
  | Add (x, y) ->
    begin
      let sum = chip8.registers.(x) + chip8.registers.(y) in
      chip8.registers.(0xf) <- if sum > 0xff then 1 else 0;
      chip8.registers.(x) <- sum mod 0x100;
      {chip8 with pc = chip8.pc+2 }
    end
  | Sub (x, y) ->
    begin
      let sub = chip8.registers.(x) - chip8.registers.(y) in
      chip8.registers.(0xf) <- if sub >= 0 then 1 else 0;
      chip8.registers.(x) <- (sub + 0x100) mod 0x100;
      {chip8 with pc = chip8.pc+2 }
    end
  | RShift (x, y) ->
    begin
      if use_shift_second_register then
        chip8.registers.(x) <- chip8.registers.(y);
      chip8.registers.(0xf) <- chip8.registers.(x) land 0x01;
      chip8.registers.(x) <- chip8.registers.(x) lsr 1;
      {chip8 with pc = chip8.pc+2 }
    end
  | MSub (x, y) ->
    begin
      let sub = chip8.registers.(y) - chip8.registers.(x) in
      chip8.registers.(0xf) <- if sub >= 0 then 1 else 0;
      chip8.registers.(x) <- (sub + 0x100) mod 0x100 ;
      {chip8 with pc = chip8.pc+2 }
    end
  | LShift (x, y) ->
    begin
      if use_shift_second_register then
        chip8.registers.(x) <- chip8.registers.(y);
      chip8.registers.(0xf) <- (chip8.registers.(x) land 0x80) lsr 7;
      chip8.registers.(x) <- (chip8.registers.(x) lsl 1) land 0xff;
      {chip8 with pc = chip8.pc+2 }
    end
  | Neq (x, y) ->
    begin
      let offset = if chip8.registers.(x) <> chip8.registers.(y) then 4 else 2 in
      {chip8 with pc = chip8.pc+offset }
    end
  | SetAddr addr ->
    {chip8 with pc = chip8.pc+2; i = addr }
  | JmpOffset (offset) ->
    {chip8 with pc = chip8.registers.(0) + offset; }
  | Rand (x, n) ->
    begin
      let v = if !unittest then 0xaa
        else Random.int 256 in
      chip8.registers.(x) <- v land n;
      {chip8 with pc = chip8.pc + 2; }
    end
  | Draw(x,y,n) ->
    begin
      List.iter ((++<) n) ~f:(fun dy ->
          List.iter ((++<) 8) ~f:(fun dx ->
              let x = (chip8.registers.(x) + dx) mod 64 in
              let y = (chip8.registers.(y) + dy) mod 32 in
              let bit = (memory.(chip8.i+dy) lsr (7 - dx)) land 0x01 in
              if chip8.gfx.(y*64 + x) lxor bit <> 0x00 then chip8.registers.(0xf) <-  1;
              chip8.gfx.(y*64 + x) <- bit
            ));
      {chip8 with pc = chip8.pc + 2; draw = true}
    end
  | SkipKey x ->
    let offset =
      if Option.is_some chip8.key && chip8.registers.(x) = Option.get chip8.key then 4
      else 2 in
    {chip8 with pc = chip8.pc + offset}
  | SkipNKey x ->
    let offset =
      if Option.is_some chip8.key && chip8.registers.(x) <> Option.get chip8.key then 4
      else 2 in
    {chip8 with pc = chip8.pc + offset}
  | GetTimer x ->
    begin
      chip8.registers.(x) <- chip8.delay_timer;
      {chip8 with pc = chip8.pc + 2}
    end
  | Key x ->
    begin
      chip8.registers.(x) <- if !unittest then 0xf else Keys.get_key ();
      {chip8 with pc = chip8.pc + 2}
    end
  | SetTimer x ->
    {chip8 with pc = chip8.pc + 2; delay_timer = chip8.registers.(x) }
  | SetSound x ->
    {chip8 with pc = chip8.pc + 2; sound_timer = chip8.registers.(x) }
  | AddInput x ->
    {chip8 with pc = chip8.pc + 2; i = chip8.i + chip8.registers.(x) }
  | SetFont x ->
    {chip8 with pc = chip8.pc + 2; i = splite_addr + chip8.registers.(x) }
  | StoreBCD  x ->
    begin
      memory.(chip8.i+0) <- chip8.registers.(x) /  100;
      memory.(chip8.i+1) <- (chip8.registers.(x) mod 100) / 10;
      memory.(chip8.i+2) <- chip8.registers.(x) mod 10;
      {chip8 with pc = chip8.pc + 2}
    end
  | Dump x ->
    begin
      List.iter ((++=) x) ~f:(fun i -> memory.(chip8.i+i) <- chip8.registers.(i));
      {chip8 with pc = chip8.pc + 2}
    end
  | Load x ->
    begin
      List.iter ((++=) x) ~f:(fun i -> chip8.registers.(i) <- memory.(chip8.i+i));
      {chip8 with pc = chip8.pc + 2}

    end

let update_timer state =
  let delay_timer =
    if state.delay_timer > 0 then pred state.delay_timer else 0 in
  let sound_timer =
    if state.sound_timer > 0 then
      begin
        (if state.sound_timer = 1 then
           Printf.printf "BEEP!");
        pred state.delay_timer
      end
    else 0
  in
  {state with delay_timer = delay_timer;
              sound_timer = sound_timer}

let emurate_cycle state =
  fetch state.pc
  |> decode
  |> execute state
  |> update_timer
