open Type
open State

type opcode =
  | DisplayClear                  (* 0x00E0 *)
  | Ret                           (* 0x00EE *)
  | Jmp of uint8                  (* 0x1NNN *)
  | Call of uint8                 (* 0x2NNN *)
  | SkipEq of uint8 * uint8       (* 0x3XNN *)
  | SkipNeq of uint8 * uint8      (* 0x4XNN *)
  | SkipCond of uint8 * uint8     (* 0x5XY0 *)
  | SetI of uint8 * uint8         (* 0x6XNN *)
  | AddI of uint8 * uint8         (* 0x7XNN *)
  | Set of uint8 * uint8          (* 0x8XY0 *)
  | Or of uint8 * uint8           (* 0x8XY1 *)
  | And of uint8 * uint8          (* 0x8XY2 *)
  | Xor of uint8 * uint8          (* 0x8XY3 *)
  | Add of uint8 * uint8          (* 0x8XY4 *)
  | Sub of uint8 * uint8          (* 0x8XY5 *)
  | RShift of uint8 * uint8       (* 0x8XY6 *)
  | MSub of uint8 * uint8         (* 0x8XY7 *)
  | LShift of uint8 * uint8       (* 0x8XYE *)
  | Neq of uint8 * uint8          (* 0x9XY0 *)
  | SetAddr of uint8              (* 0xANNN *)
  | JmpOffset of uint8            (* 0xBNNN *)
  | Rand of uint8 * uint8         (* 0xCXNN *)
  | Draw of uint8 * uint8 * uint8 (* 0xDXYN *)
  | SkipKey of uint8              (* 0xEX9E *)
  | SkipNKey of uint8             (* 0xEXA1 *)
  | GetTimer of uint8             (* 0xFX07 *)
  | Key of uint8                  (* 0xFX0A *)
  | SetTimer of uint8             (* 0xFX15 *)
  | SetSound of uint8             (* 0xFX18 *)
  | AddInput of uint8             (* 0xFX1E *)
  | SetFont of uint8              (* 0xFX29 *)
  | StoreBCD of uint8             (* 0xFX33 *)
  | Dump of uint8                 (* 0xFX55 *)
  | Load of uint8                 (* 0xFX65 *)

let print (state:chip8) opcode =
  match opcode with
  | DisplayClear -> "display_clear()"
  | Ret -> "return"
  | Jmp addr -> Printf.sprintf "goto 0x%x" addr
  | Call addr -> Printf.sprintf "*(0x%x)()" addr
  | SkipEq (x, n)-> Printf.sprintf "skip if V(%d)[0x%x] = 0x%x" x state.registers.(x) n
  | SkipNeq (x, n) -> Printf.sprintf "skip if V(%d) <> 0x%x" x n
  | SkipCond (x, y) -> Printf.sprintf "skip if V(%d) = V(%d)" x y
  | SetI (x,n) -> Printf.sprintf "V(%d)[0x%x] = 0x%x" x state.registers.(x) n
  | AddI (x,n) -> Printf.sprintf "V(%d) += 0x%x" x n
  | Set (x,y) -> Printf.sprintf "V(%d) = V(%d)" x y
  | Or (x, y) -> Printf.sprintf "V(%d) = V(%d) | V(%d)" x x y
  | And (x, y) -> Printf.sprintf "V(%d) = V(%d) & V(%d)" x x y
  | Xor (x, y) -> Printf.sprintf "V(%d) = V(%d) ^ V(%d)" x x y
  | Add (x, y) -> Printf.sprintf "V(%d) += V(%d)" x y
  | Sub (x, y) -> Printf.sprintf "V(%d)[0x%x] -= V(%d)[0x%x]" x state.registers.(x) y state.registers.(y)
  | RShift (x , _) -> Printf.sprintf "V(%d) >>= 1" x
  | MSub (x, y) -> Printf.sprintf "V(%d) = V(%d) - V(%d)" x y x
  | LShift (x, _) -> Printf.sprintf "V(%d) <<= 1" x
  | Neq (x, y) -> Printf.sprintf "skip if V(%d) <> V(%d)" x y
  | SetAddr addr -> Printf.sprintf "i = 0x%x" addr
  | JmpOffset n -> Printf.sprintf "JumpOffset +%d" n
  | Draw (x, y, n) -> Printf.sprintf "draw(V(%d), V(%d), %d)" x y n
  | StoreBCD x -> Printf.sprintf "store bcd at V(%d)" x
  | Dump x -> Printf.sprintf "dump from V(0) to V(%d)" x
  | Load x -> Printf.sprintf "store from V(0) to V(%d)" x
  | _ -> ""
