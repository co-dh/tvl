/-
  Fzf: helpers for running fzf picker and bat viewer
  All fzf functions take testMode: if true, return first/default without spawning fzf
-/
import Tv.Term
import Tv.Types

namespace App

-- | Core fzf: testMode returns first line, else spawn fzf
private def fzfCore (opts : Array String) (input : String) (testMode : Bool) : IO String :=
  if testMode then pure (input.splitOn "\n" |>.filter (!·.isEmpty) |>.headD "")
  else do
    Term.shutdown
    let args := #["--height=50%", "--layout=reverse"] ++ opts
    let child ← IO.Process.spawn { cmd := "fzf", args, stdin := .piped, stdout := .piped }
    child.stdin.putStr input
    child.stdin.flush
    let (_, child') ← child.takeStdin
    let out ← child'.stdout.readToEnd
    let _ ← child'.wait
    let _ ← Term.init
    pure out.trim

-- | Single select
def fzf (opts : Array String) (input : String) (testMode : Bool := false) : IO (Option String) := do
  let out ← fzfCore opts input testMode
  pure (if out.isEmpty then none else some out)

-- | Multi select. testMode: first line as singleton.
def fzfMulti (opts : Array String) (input : String) (testMode : Bool := false) : IO (Array String) := do
  let out ← fzfCore (#["-m"] ++ opts) input testMode
  pure (if testMode then (if out.isEmpty then #[] else #[out])
        else out.splitOn "\n" |>.map String.trim |>.filter (!·.isEmpty) |>.toArray)

-- | Index select. testMode: ⟨0⟩.
def fzfIdx (opts : Array String) (items : Array String) (testMode : Bool := false) : IO (Option DispIdx) :=
  if testMode then pure (if items.isEmpty then none else some ⟨0⟩)
  else do
    let numbered := items.mapIdx fun i s => s!"{i}\t{s}"
    let out ← fzfCore (#["--with-nth=2.."] ++ opts) (numbered.join "\n") false
    if out.isEmpty then return none
    match out.splitOn "\t" |>.head? |>.bind String.toNat? with
    | some n => return some ⟨n⟩
    | none => return none

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

end App
