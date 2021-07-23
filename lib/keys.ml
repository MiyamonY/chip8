open Tsdl

let setup_input () = ()

let keymap (k:Sdl.scancode) =
  match (Sdl.Scancode.enum k)  with
  | `K0 -> Some 0x0
  | `K1 -> Some 0x1
  | `K2 -> Some 0x2
  | `K3 -> Some 0x3
  | `K4 -> Some 0x4
  | `K5 -> Some 0x5
  | `K6 -> Some 0x6
  | `K7 -> Some 0x7
  | `K8 -> Some 0x8
  | `K9 -> Some 0x9
  | `A -> Some 0xa
  | `B -> Some 0xb
  | `C -> Some 0xc
  | `D -> Some 0xd
  | `E -> Some 0xe
  | `F -> Some 0xf
  |_ -> None

let set_keys (state:State.chip8) =
  let event = Sdl.Event.create () in
  if Sdl.poll_event @@ Some event then
    match Sdl.Event.(get event typ |>  enum) with
    | `Key_down ->
      {state with key = keymap @@ Sdl.Event.(get event keyboard_scancode)  }
    | _ -> {state with key = None}
  else state

let get_key () =
  let rec loop () =
    let result =
      let event = Sdl.Event.create () in
      match Sdl.wait_event @@ Some event with
      | Error (`Msg _) -> None
      | Ok () ->
        match Sdl.Event.(get event typ |>  enum) with
        | `Key_down ->
          keymap @@ Sdl.Event.(get event keyboard_scancode)
        | _ -> None
    in
    match result with
    | Some key -> key
    | None -> loop ()
  in
  loop ()
