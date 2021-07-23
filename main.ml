open StdLabels
open Chip8

let rec loop chip8 =
  Cycle.emurate_cycle chip8
  |> Graphics.draw_graphics (* (fun chip8 -> if chip8.draw then Graphics.draw_graphics chip8 else chip8) *)
  |> Keys.set_keys
  |> loop

let () =
  let (window, renderer) = Graphics.setup_graphics () in
  Keys.setup_input ();
  Memory.load_game Sys.argv.(1);
  Memory.initialize_memory ();
  try
    loop {pc = 0x200; i = 0; draw=false; delay_timer=0; sound_timer=0; stack = Stack.create ();
          registers = Array.init 16 ~f:(fun _ -> 0);
          gfx = Array.init (64*32) ~f:(fun _ -> 0);
          key = None;
          window = window;
          renderer = renderer
         } |> ignore with
  | Failure e ->
    let rec infinite_loop () = infinite_loop () in
    Printf.eprintf "error %s" e;
    flush stdout;
    flush stderr;
    infinite_loop ()
