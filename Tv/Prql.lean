/-
  Type-safe PRQL statement constructor
-/

namespace Prql

-- | Sort direction
inductive Dir where | asc | desc
  deriving Inhabited, BEq

-- | Aggregate function
inductive Agg where
  | count | sum | avg | min | max | stddev
  deriving Inhabited, BEq

-- | PRQL operation (single pipe stage)
inductive Op where
  | filter (expr : String)                         -- filter <expr>
  | sort (cols : List (String × Dir))              -- sort {col, -col2}
  | sel (cols : List String)                       -- select {a, b, c}
  | derive (bindings : List (String × String))     -- derive {x = expr}
  | group (keys : List String) (aggs : List (Agg × String × String))  -- group {k} (agg {name = fn col})
  | freq (col : String)                            -- freq col (builtin)
  | take (n : Nat)                                 -- take n
  deriving Inhabited

-- | PRQL query: base table + list of operations
structure Query where
  base : String := "from df"
  ops  : List Op := []
  deriving Inhabited

-- | Reserved PRQL names needing this. prefix
def reserved : List String :=
  ["count", "sum", "avg", "min", "max", "average", "group", "sort",
   "filter", "select", "derive", "from", "take", "date", "time"]

-- | Quote column name (add this. prefix for reserved words)
def quote (s : String) : String :=
  if reserved.contains s then s!"this.{s}" else s

-- | Render sort direction
def Dir.render (d : Dir) (col : String) : String :=
  match d with | .asc => col | .desc => s!"-{col}"

-- | Render aggregate function name
def Agg.name : Agg → String
  | .count => "count" | .sum => "sum" | .avg => "average"
  | .min => "min" | .max => "max" | .stddev => "stddev"

-- | Render single operation to PRQL string
def Op.render : Op → String
  | .filter e => s!"filter {e}"
  | .sort cols =>
    let cs := cols.map fun (c, d) => d.render c
    s!"sort \{{String.intercalate ", " cs}}"
  | .sel cols => s!"select \{{String.intercalate ", " (cols.map quote)}}"
  | .derive bs =>
    let pairs := bs.map fun (n, e) => s!"{quote n} = {e}"
    s!"derive \{{String.intercalate ", " pairs}}"
  | .group keys aggs =>
    let ks := String.intercalate ", " keys
    let as := aggs.map fun (fn, name, col) => s!"{name} = {fn.name} {col}"
    s!"group \{{ks}} (aggregate \{{String.intercalate ", " as}})"
  | .freq col => s!"freq {col}"
  | .take n => s!"take {n}"

-- | Render full query to PRQL string
def Query.render (q : Query) : String :=
  if q.ops.isEmpty then q.base
  else q.base ++ " | " ++ String.intercalate " | " (q.ops.map Op.render)

-- | Parse base query string to Query (extracts existing ops)
def Query.parse (s : String) : Query :=
  -- for now, just wrap the string as base (TODO: full parser)
  ⟨s, []⟩

-- | Pipe: append operation to query
def Query.pipe (q : Query) (op : Op) : Query :=
  { q with ops := q.ops ++ [op] }

-- | Convenient operators
instance : HAppend Query Op Query where hAppend := Query.pipe

-- | Builder helpers
def Query.new (tbl : String := "df") : Query := ⟨s!"from {tbl}", []⟩

def Query.filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)
def Query.sortAsc (q : Query) (col : String) : Query := q.pipe (.sort [(col, .asc)])
def Query.sortDesc (q : Query) (col : String) : Query := q.pipe (.sort [(col, .desc)])
def Query.select (q : Query) (cols : List String) : Query := q.pipe (.sel cols)
def Query.derive1 (q : Query) (name expr : String) : Query := q.pipe (.derive [(name, expr)])
def Query.freq (q : Query) (col : String) : Query := q.pipe (.freq col)
def Query.take (q : Query) (n : Nat) : Query := q.pipe (.take n)

-- | Frequency query with percentage bar (common pattern)
def Query.freqFull (q : Query) (cols : List String) : Query :=
  let grp : Op := .group cols [(.count, "Cnt", "this")]
  let pct : Op := .derive [("Pct", "Cnt * 100 / sum Cnt"),
                           ("Bar", "s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"")]
  let srt : Op := .sort [("Cnt", .desc)]
  { q with ops := q.ops ++ [grp, pct, srt] }

-- | Aggregate query (group by keys, apply funcs to cols)
def Query.agg (q : Query) (keys : List String) (funcs : List Agg) (cols : List String) : Query :=
  let aggs := funcs.flatMap fun f => cols.map fun c => (f, s!"{f.name}_{c}", c)
  q.pipe (.group keys aggs)

end Prql
