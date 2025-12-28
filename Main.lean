/-
  tv-lean: Tabular viewer in Lean 4
  Usage: tv <file> [--keys "jjjF<ret>q"]
-/
import Tv.App

-- | Parse key notation: <ret> -> \r, <esc> -> \x1b, <C-d> -> Ctrl-D, etc.
def parseKeys (s : String) : String :=
  s.replace "<ret>" "\r"
   |>.replace "<esc>" "\x1b"
   |>.replace "<C-d>" "\x04"
   |>.replace "<C-u>" "\x15"
   |>.replace "<backslash>" "\\"

-- | Parse args: (path, keys, testMode)
def parseArgs : List String → Option (String × String × Bool)
  | [path] => some (path, "", false)
  | [path, "--keys", keys] => some (path, parseKeys keys, true)
  | ["--keys", keys, path] => some (path, parseKeys keys, true)
  | _ => none

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | some (path, keys, testMode) => App.run path keys testMode
  | none => IO.eprintln "Usage: tv <file> [--keys \"jjjF<ret>q\"]"
