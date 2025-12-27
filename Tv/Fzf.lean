/-
  Fzf: helpers for running fzf picker and bat viewer
-/
import Tv.Term
import Tv.Types

namespace App

-- | Run fzf picker (suspends terminal, shows screen buffer at top)
def runFzf (opts : List String) (input : String) (header : String := "") : IO (Option String) := do
  -- capture screen buffer before shutdown
  let screen ← if header.isEmpty then Term.bufferStr else pure header
  Term.shutdown
  -- print captured screen at top
  IO.print screen
  let child ← IO.Process.spawn {
    cmd := "fzf"
    args := ("--height=50%" :: "--layout=reverse" :: opts).toArray
    stdin := .piped
    stdout := .piped
  }
  child.stdin.putStr input
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let out ← child'.stdout.readToEnd
  let _ ← child'.wait
  let _ ← Term.init
  let result := out.trim
  return if result.isEmpty then none else some result

-- | Run bat to display file (suspends terminal)
def runBat (path : String) : IO Unit := do
  Term.shutdown
  let _ ← IO.Process.spawn {
    cmd := "bat"
    args := #["--paging=always", path]
    stdin := .inherit
    stdout := .inherit
  } >>= (·.wait)
  let _ ← Term.init

-- | Run fzf with numbered items, return selected index
def runFzfIdx (opts : List String) (items : Array String) : IO (Option DispIdx) := do
  Term.shutdown
  let numbered := items.mapIdx fun i s => s!"{i}\t{s}"
  let child ← IO.Process.spawn {
    cmd := "fzf"
    args := ("--with-nth=2.." :: "--height=50%" :: "--layout=reverse" :: opts).toArray
    stdin := .piped
    stdout := .piped
  }
  child.stdin.putStr (String.intercalate "\n" numbered.toList)
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let out ← child'.stdout.readToEnd
  let _ ← child'.wait
  let _ ← Term.init
  let line := out.trim
  if line.isEmpty then return none
  match line.splitOn "\t" |>.head? |>.bind String.toNat? with
  | some n => return some ⟨n⟩
  | none => return none

-- | Run fzf multi-select
def runFzfMulti (opts : List String) (input : String) : IO (List String) := do
  Term.shutdown
  let child ← IO.Process.spawn {
    cmd := "fzf"
    args := ("-m" :: opts).toArray
    stdin := .piped
    stdout := .piped
  }
  child.stdin.putStr input
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let out ← child'.stdout.readToEnd
  let _ ← child'.wait
  let _ ← Term.init
  return out.splitOn "\n" |>.map String.trim |>.filter (!·.isEmpty)

end App
