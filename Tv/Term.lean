/-
  termbox2 FFI bindings for TUI rendering
-/

namespace Term

-- | Key codes
def keyArrowUp    : UInt16 := 0xFFFF - 18
def keyArrowDown  : UInt16 := 0xFFFF - 19
def keyArrowLeft  : UInt16 := 0xFFFF - 20
def keyArrowRight : UInt16 := 0xFFFF - 21
def keyEsc        : UInt16 := 0x1B
def keyEnter      : UInt16 := 0x0D

-- | Event types
def eventKey    : UInt8 := 1
def eventResize : UInt8 := 2

-- | Colors (basic)
def black   : UInt32 := 0x000000
def white   : UInt32 := 0xFFFFFF
def cyan    : UInt32 := 0x00FFFF
def yellow  : UInt32 := 0xFFFF00

-- | Terminal event from poll
structure Event where
  type : UInt8
  mod  : UInt8
  key  : UInt16
  ch   : UInt32
  w    : UInt32  -- resize width
  h    : UInt32  -- resize height
  deriving Repr

-- FFI declarations
@[extern "lean_tb_init"]
opaque init : IO Int32

@[extern "lean_tb_shutdown"]
opaque shutdown : IO Unit

@[extern "lean_tb_width"]
opaque width : IO UInt32

@[extern "lean_tb_height"]
opaque height : IO UInt32

@[extern "lean_tb_clear"]
opaque clear : IO Unit

@[extern "lean_tb_present"]
opaque present : IO Unit

@[extern "lean_tb_set_cell"]
opaque setCell : UInt32 → UInt32 → UInt32 → UInt32 → UInt32 → IO Unit

@[extern "lean_tb_poll_event"]
opaque pollEvent : IO Event

-- | Print string at position with colors
def print (x y : UInt32) (fg bg : UInt32) (s : String) : IO Unit := do
  let mut cx := x
  for c in s.toList do
    setCell cx y c.toNat.toUInt32 fg bg
    cx := cx + 1

-- | Print string, truncated/padded to width
def printPad (x y w : UInt32) (fg bg : UInt32) (s : String) : IO Unit := do
  let padded := s.take w.toNat ++ String.ofList (List.replicate (w.toNat - min s.length w.toNat) ' ')
  print x y fg bg padded

end Term
