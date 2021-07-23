open Type
open Tsdl

type chip8 = {
  pc : uint8;
  i: uint8;                     (* index register *)
  draw: bool;
  delay_timer:uint8;
  sound_timer: uint8;
  stack: uint8 Stack.t;
  registers: uint8 array;
  gfx: uint8 array;
  key: uint8 option;
  window: Sdl.window;
  renderer: Sdl.renderer
}
