/-
  Qtv.Key - Key bindings matching Q version
  Q: .kf namespace with KEY_* handlers
-/
import Qtv.Render

namespace Qtv

-- | Key codes (matching termbox/curses)
inductive Key where
  | char (c : Char)
  | down | up | left | right
  | pgdn | pgup
  | ctrl (c : Char)
  deriving Repr, BEq

namespace Key

-- | Parse key name to Key
-- Q: kn::`$"c"$$[0=cnt;"";keyname getch[]]
def ofName : String → Option Key
  | "KEY_DOWN" => some .down
  | "KEY_UP" => some .up
  | "KEY_LEFT" => some .left
  | "KEY_RIGHT" => some .right
  | "kDN5" => some .pgdn
  | "kUP5" => some .pgup
  | s => if s.length == 1 then some (.char s.front)
         else if s.startsWith "^" && s.length == 2 then
           some (.ctrl s.back)
         else none

end Key

namespace State

-- | Handle key, returns updated state
-- Q: .kf[key][]
def onKey (s : State) (k : Key) : State :=
  match k with
  -- navigation
  -- Q: .kf.KEY_DOWN, .kf.KEY_UP, .kf.KEY_LEFT, .kf.KEY_RIGHT
  | .down => s.navDown
  | .up => s.navUp
  | .left => s.navLeft
  | .right => s.navRight
  | .pgdn => s.navPgDn
  | .pgup => s.navPgUp
  -- Q: .kf[`$"^D"], .kf[`$"^U"]
  | .ctrl 'D' => s.navPgDn
  | .ctrl 'U' => s.navPgUp
  -- Q: vim-style hjkl
  | .char 'j' => s.navDown
  | .char 'k' => s.navUp
  | .char 'h' => s.navLeft
  | .char 'l' => s.navRight
  -- Q: .kf.g, .kf.G
  | .char 'g' => s.navTop
  | .char 'G' => s.navBot
  -- column ops
  -- Q: .kf.d, .kf.c, .kf[`$"["], .kf[`$"]"], .kf[`$"!"]
  | .char 'd' => s.opDel
  | .char 'c' => s.opCp
  | .char '[' => s.opAsc
  | .char ']' => s.opDesc
  | .char '!' => s.opBang
  -- views
  -- Q: .kf.F, .kf.M, .kf.D, .kf.S, .kf.q
  | .char 'F' => s.opFreq
  | .char 'M' => s.opMeta
  | .char 'D' => s.dup
  | .char 'S' => s.swap
  | .char 'q' => s.pop
  -- search
  -- Q: .kf.n, .kf.N, .kf[`$"*"]
  | .char 'n' => match s.getReg '/' with
    | some p => s.searchFwd p
    | none => s
  | .char 'N' => match s.getReg '/' with
    | some p => s.searchBwd p
    | none => s
  | .char '*' => s.searchCur
  | _ => s

-- | Handle key with input mode
-- Q: input, inputT, fzf integration
inductive InputReq where
  | none
  | search           -- Q: .kf[`$"/"]
  | filter           -- Q: .kf[`$"\\"]
  | rename           -- Q: .kf[`$"^"]
  | typeConvert      -- Q: .kf[`$"$"]
  | agg              -- Q: .kf.b
  deriving Repr

def onKeyInput (s : State) (k : Key) : State × InputReq :=
  match k with
  | .char '/' => (s, .search)
  | .char '\\' => (s, .filter)
  | .char '^' => (s, .rename)
  | .char '$' => (s, .typeConvert)
  | .char 'b' => (s, .agg)
  | _ => (s.onKey k, .none)

-- | Complete input and update state
def completeInput (s : State) (req : InputReq) (input : String) : State :=
  match req with
  | .search => (s.setReg '/' input).searchFwd input
  | .filter => s.opFlt input
  | .rename => s.opRen input
  | _ => s

end State

end Qtv
