/-
  Type-safe PRQL statement constructor
-/
import Tv.Types

namespace Prql

-- | Aggregate function (for PRQL group/agg)
inductive Agg where
  | count | sum | avg | min | max | stddev
  deriving Repr, Inhabited

-- | PRQL operation (single pipe stage)
inductive Op where
  | filter (expr : String)                          -- filter <expr>
  | sort (cols : Array (String × Bool))             -- sort {col, -col2}; Bool = asc
  | sel (cols : Array String)                       -- select {a, b, c}
  | derive (bindings : Array (String × String))     -- derive {x = expr}
  | group (keys : Array String) (aggs : Array (Agg × String × String))  -- group {k} (agg {name = fn col})
  | take (n : Nat)                                  -- take n
  deriving Inhabited

-- | PRQL query: base table + operations
structure Query where
  base : String := "from df"
  ops  : Array Op := #[]
  deriving Inhabited

-- | Reserved PRQL names needing this. prefix
def reserved : Array String :=
  #["count", "sum", "avg", "min", "max", "average", "group", "sort",
    "filter", "select", "derive", "from", "take", "date", "time"]

-- | Quote column name (backticks for special chars, this. for reserved)
def quote (s : String) : String :=
  let needsBacktick := s.any fun c => !c.isAlphanum && c != '_'
  if needsBacktick then s!"`{s}`"
  else if reserved.contains s then s!"this.{s}"
  else s

-- | Render sort column (asc = col, desc = -col)
def renderSort (col : String) (asc : Bool) : String :=
  let qc := quote col
  if asc then qc else s!"-{qc}"

-- | Render aggregate function name (std. prefix to avoid column name conflicts)
def Agg.name : Agg → String
  | .count => "std.count" | .sum => "std.sum" | .avg => "std.average"
  | .min => "std.min" | .max => "std.max" | .stddev => "std.stddev"

-- | Short name for result column (no std. prefix)
def Agg.short : Agg → String
  | .count => "count" | .sum => "sum" | .avg => "average"
  | .min => "min" | .max => "max" | .stddev => "stddev"

-- | Render single operation to PRQL string
def Op.render : Op → String
  | .filter e => s!"filter {e}"
  | .sort cols => s!"sort \{{(cols.map fun (c, asc) => renderSort c asc).join ", "}}"
  | .sel cols => s!"select \{{(cols.map quote).join ", "}}"
  | .derive bs => s!"derive \{{(bs.map fun (n, e) => s!"{quote n} = {e}").join ", "}}"
  | .group keys aggs =>
    let as := aggs.map fun (fn, name, col) => s!"{name} = {fn.name} {quote col}"
    s!"group \{{(keys.map quote).join ", "}} (aggregate \{{as.join ", "}})"
  | .take n => s!"take {n}"

-- | Render full query to PRQL string
def Query.render (q : Query) : String :=
  if q.ops.isEmpty then q.base
  else q.base ++ " | " ++ (q.ops.map Op.render).join " | "

-- | Pipe: append operation to query
def Query.pipe (q : Query) (op : Op) : Query := { q with ops := q.ops.push op }

-- | Convenient operators
instance : HAppend Query Op Query where hAppend := Query.pipe
infixl:65 " |> " => Query.pipe  -- q |> .filter "x > 5"

-- | Builder helpers
def Query.filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)
def Query.select (q : Query) (cols : Array String) : Query := q.pipe (.sel cols)
def Query.derive1 (q : Query) (name expr : String) : Query := q.pipe (.derive #[(name, expr)])

-- | Frequency query with percentage bar
def Query.freq (q : Query) (cols : Array String) : Query :=
  let grp : Op := .group cols #[(.count, "Cnt", "this")]
  let pct : Op := .derive #[("Pct", "Cnt * 100 / std.sum Cnt"),
                            ("Bar", "s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"")]
  let srt : Op := .sort #[("Cnt", false)]  -- desc
  { q with ops := q.ops ++ #[grp, pct, srt] }

-- | Aggregate query (group by keys, apply funcs to cols)
def Query.agg (q : Query) (keys : Array String) (funcs : Array Agg) (cols : Array String) : Query :=
  let aggs := funcs.flatMap fun f => cols.map fun c => (f, s!"{f.short}_{c}", c)
  q.pipe (.group keys aggs)

end Prql
