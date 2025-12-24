/-
  tv-lean: Tabular viewer in Lean 4
  Usage: tv <file.csv>
-/
import Tv.App

def main (args : List String) : IO Unit := do
  match args with
  | [path] => App.run path
  | _ => IO.eprintln "Usage: tv <file.csv>"
