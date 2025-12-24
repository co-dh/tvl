/-
  tv-lean: Tabular viewer in Lean 4
  Usage: tv <file> [--keys "jjjFq"]
-/
import Tv.App

-- | Parse args: file path and optional --keys
def parseArgs : List String → Option (String × String)
  | [path] => some (path, "")
  | [path, "--keys", keys] => some (path, keys)
  | ["--keys", keys, path] => some (path, keys)
  | _ => none

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | some (path, keys) => App.run path keys
  | none => IO.eprintln "Usage: tv <file> [--keys \"jjjFq\"]"
