open StdLabels
open Chip8.Cycle
open Chip8.Memory
open Chip8.Graphics
open Chip8.Util

let (window, renderer) = setup_graphics ()

let run binary_code =
  let stack = Stack.create () in
  Stack.push 0xfff stack;
  List.iter ((++=) 5) ~f:(fun i -> memory.(i) <- i);
  memory.(0xf0f) <- 0x100;
  decode binary_code
  |> execute {pc = 0x200; i = 0; draw=false; delay_timer=0x55; sound_timer=0xaa; stack = stack;
              registers = Array.init 16 ~f:(fun i -> if i = 0xf then 0xff else if i = 0xd then 0x80 else i);
              gfx = Array.init (64*32) ~f:(fun _ -> 0xff);
              key = Some 0x0b;
              window = window;
              renderer = renderer
             }

let%test "clear dispaly" =
  let state = run 0x00E0 in
  Array.for_all ~f:((=) 0)  state.gfx

let%test "return from a subroutine" =
  let state = run 0x00EE in
  state.pc = 0xfff

let%test "goto" =
  let state = run 0x1f0f in
  state.pc = 0xf0f

let%test "call a subroutine" =
  let state = run 0x2f0f in
  let pushed_pc = Stack.top state.stack in
  state.pc = 0xf0f && pushed_pc = 0x202

let%test_module "skip if same immidiate value" = (module struct
  let%test "same value" =
    let state = run 0x3000 in
    state.pc = 0x204

  let%test "different value" =
    let state = run 0x3001 in
    state.pc = 0x202
end)

let%test_module "skip if different immidiate value" = (module struct
  let%test "same value" =
    let state = run 0x4101 in
    state.pc = 0x202

  let%test "different value" =
    let state = run 0x4100 in
    state.pc = 0x204
end)

let%test_module "skip if same register value" = (module struct
  let%test "same value" =
    let state = run 0x5220 in
    state.pc = 0x204

  let%test "different value" =
    let state = run 0x5010 in
    state.pc = 0x202
end)

let%test "set to register" =
  let state = run 0x61ff in
  state.pc = 0x202 && state.registers.(1) = 0xff

let%test "add to register (0x7XNN)" =
  let state = run 0x72ff in
  state.pc = 0x202 && state.registers.(2) = 0x01 && state.registers.(0xf) = 0xff

let%test "assign (0x8XY0)" =
  let state = run 0x8430 in
  state.pc = 0x202 && state.registers.(4) = 0x03

let%test "bit or (0x8XY1)" =
  let state = run 0x8a51 in
  state.pc = 0x202 && state.registers.(0xa) = 0xf

let%test "bit and (0x8XY2)" =
  let state = run 0x8a52 in
  state.pc = 0x202 && state.registers.(0xa) = 0x00

let%test "bit xor (0x8XY3)" =
  let state = run 0x8af3 in
  state.pc = 0x202 && state.registers.(0xa) = 0xf5

let%test "add (0x8XY4)" =
  let state = run 0x89f4 in
  state.pc = 0x202 && state.registers.(0x9) = 0x08 && state.registers.(0xf) = 0x01

let%test_module "sub (0x8XY5)" = (module struct
  let%test "occur borrow" =
    let state = run 0x8135 in
    state.pc = 0x202 && state.registers.(0x1) = 0xfe && state.registers.(0xf) = 0x00

  let%test "not occur borrow" =
    let state = run 0x8315 in
    state.pc = 0x202 && state.registers.(0x3) = 0x02 && state.registers.(0xf) = 0x01
end)

let%test_module "bit right shift (0x8XY6)" = (module struct
  let%test "shift overflow" =
    let state = run 0x8146 in
    state.pc = 0x202 && state.registers.(0x1) = 0x00 && state.registers.(0xf) = 0x01

  let%test "shift not overflow" =
    let state = run 0x84a6 in
    state.pc = 0x202 && state.registers.(0x4) = 0x02 && state.registers.(0xf) = 0x00
end)

let%test_module "sub minus (0x8XY7)" =(module struct
  let%test "occur borrow" =
    let state = run 0x8457 in
    state.pc = 0x202 && state.registers.(0x4) = 0x01 && state.registers.(0xf) = 0x01

  let%test "not occur borrow" =
    let state = run 0x8657 in
    state.pc = 0x202 && state.registers.(0x6) = 0xff && state.registers.(0xf) = 0x00
end)

let%test_module "bit left shift (0x8XYE)" = (module struct
  let%test "shift overflow" =
    let state = run 0x8dfE in
    state.pc = 0x202 && state.registers.(0xd) = 0x00 && state.registers.(0xf) = 0x01

  let%test "shift not overflow" =
    let state = run 0x845E in
    state.pc = 0x202 && state.registers.(0x4) = 0x08 && state.registers.(0xf) = 0x00
end)

let%test_module "skip if different register value (0x9XY0)" = (module struct
  let%test "same value" =
    let state = run 0x9220 in
    state.pc = 0x202

  let%test "different value" =
    let state = run 0x9010 in
    state.pc = 0x204
end)

let%test "set address (0xANNN)" =
  let state = run 0xafff in
  state.pc = 0x202 && state.i = 0xfff

let%test "jump offset address (0xBNNN)" =
  let state = run 0xbfff in
  state.pc = 0x0fff

let%test "rand (0xCXNN)" =
  unittest := true;
  let state = run 0xc2f0 in
  state.pc = 0x202 && state.registers.(0x2) = 0xa0

let%test "draw (0xDXYN)" =
  let state = run 0xd844 in
  state.pc = 0x202 && state.registers.(0xf) = 0x01 &&
  (* TODO: check more value *)
  state.gfx.(8+64*4) = 0 &&
  state.gfx.(8+64*5+6) = 0 &&  state.gfx.(8+64*5+7) = 1 &&
  state.gfx.(8+64*6+5) = 0 && state.gfx.(8+64*6+6) = 1 &&  state.gfx.(8+64*6+7) = 0

let%test_module "skip if same key value (0xEX9E)" = (module struct
  let%test "same value" =
    let state = run 0xeb9e in
    state.pc = 0x204

  let%test "different value" =
    let state = run 0xe89e in
    state.pc = 0x202
end)

let%test_module "skip if different key value (0xEXA1)" = (module struct
  let%test "same value" =
    let state = run 0xeba1 in
    state.pc = 0x202

  let%test "different value" =
    let state = run 0xe8a1 in
    state.pc = 0x204
end)

let%test "get timer (0xFX07)" =
  let state = run 0xf607 in
  state.pc = 0x202 && state.registers.(0x6) = 0x55

let%test "get key (0xFX0A)" =
  let state = run 0xf70A in
  state.pc = 0x202 && state.registers.(0x7) = 0x0f

let%test "set delaytimer (0xFX15)" =
  let state = run 0xfe15 in
  state.pc = 0x202 && state.delay_timer = 0x0e

let%test "set sound timer (0xFX18)" =
  let state = run 0xfc18 in
  state.pc = 0x202 && state.sound_timer = 0x0c

let%test "add to i (0xFX1E)" =
  let state = run 0xff1E in
  state.pc = 0x202 && state.i = 0xff

let%test "set font (0xFX29)" =
  let state = run 0xfb29 in
  state.pc = 0x202 && state.i = splite_addr + 0x0b

let%test "set bcd (0xFX33)" =
  let state = run 0xff33 in
  state.pc = 0x202 && memory.(state.i) = 2 && memory.(state.i+1) = 5 && memory.(state.i+2)= 5

let%test "dump regsiter (0xFX55)" =
  let state = run 0xfa55 in
  state.pc = 0x202 && List.for_all ((++=) 0xa) ~f:(fun i-> memory.(state.i + i) = i)

let%test "load regsiter (0xFX65)" =
  let state = run 0xfa65 in
  state.pc = 0x202 && List.for_all ((++=) 0xa) ~f:(fun i-> state.registers.(i) = i)
