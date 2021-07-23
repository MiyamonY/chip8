open StdLabels
open Tsdl

let setup_graphics () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e;
    exit 1
  | Ok () ->
    match Sdl.create_window "Chip8" ~w:640 ~h:320 (Sdl.Window.shown) with
    | Error (`Msg e) ->
      Sdl.log "Create window error: %s" e;
      exit 1
    | Ok window ->
      match Sdl.create_renderer ~flags:Sdl.Renderer.presentvsync window with
      | Error (`Msg e)-> Sdl.log "%s" e; exit 1
      | Ok renderer ->
        (window, renderer)

let draw_graphics (state: State.chip8) =
  let clear () =
    Sdl.set_render_draw_color state.renderer 0x00 0x00 0x00 0xFF |> ignore;
    Sdl.render_clear state.renderer |> ignore
  in
  if state.draw then
    begin
      clear ();
      Sdl.set_render_draw_color state.renderer 0xFF 0xFF 0xFF 0xFF |> ignore;
      Array.iteri state.gfx
        ~f:(fun i v ->
            if v > 0 then
              begin
                let rect = Sdl.Rect.create ~x:(10 * (i mod 64)) ~y:(10 * (i / 64)) ~w:10 ~h:10 in
                Sdl.render_fill_rect state.renderer (Some rect) |> ignore;
                Sdl.render_draw_rect state.renderer (Some rect) |> ignore
              end
          );
      Sdl.render_present state.renderer |> ignore
    end;
  { state with draw = false }
