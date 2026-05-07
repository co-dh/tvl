# Logica Cheat Sheet

Google's Logica is a logic programming language that compiles to SQL. It runs on BigQuery, PostgreSQL, SQLite, and DuckDB.

```bash
pip install logica
logica file.l run PredicateName
```

---

## Core Syntax

### Facts (base data)
```logica
Color("red").
Color("blue").
Book("Dune", 9.99).
```

### Rules
```logica
Head(args) :- Body1(args), Body2(args);
```

The `:-` separates conclusion (head) from conditions (body). `,` means AND.

```logica
ExpensiveBook(title) :- Book(title, price), price > 50;
```

### Named columns
Logica uses named columns. Use `col: value` syntax to name output fields:

```logica
BookInfo(title: t, price: p) :- Book(t, p);
```

---

## Variables & Unification

- Lowercase or starting with uppercase — variables are unified across the rule body.
- `_` is a wildcard (anonymous variable, used when value is irrelevant).

```logica
HasBook(author) :- Wrote(author, _);
```

---

## Operators

| Category   | Operators                        |
|------------|----------------------------------|
| Arithmetic | `+`  `-`  `*`  `/`  `%`        |
| Comparison | `=`  `!=`  `<`  `>`  `<=`  `>=` |
| Logical    | `,` (AND)   `;` (OR)   `~` (NOT) |
| String     | `++` (concatenation)             |

---

## Aggregation

Logica's name comes from "Logic + Aggregation". Use `+= Count()`, `+= Sum()`, etc.

```logica
# Count books per author
BookCount(author:, count += 1) :- Wrote(author, _);

# Sum and average
Stats(total += price, avg? = Avg(price)) :- Book(_, price);
```

### Aggregation operators

| Operator         | Description                    |
|------------------|--------------------------------|
| `+= 1`           | Count (increment)              |
| `+= value`       | Sum                            |
| `max= value`     | Maximum                        |
| `min= value`     | Minimum                        |
| `? = Avg(v)`     | Average                        |
| `? = Count(v)`   | Count distinct via `Count()`   |
| `List= value`    | Collect into list              |

```logica
# Group-by is implicit: non-aggregated args become grouping keys
StatePopulation(state:, pop += 1) :- Resident(state, _);
```

---

## Negation

Use `~` for negation-as-failure (stratified negation):

```logica
Number(x) :- x in Range(10), x > 0;
Composite(a * b) :- Number(a), Number(b), a > 1, b > 1;
Prime(n) :- Number(n), n > 1, ~Composite(n);
```

---

## Built-in Predicates & Functions

### Range
```logica
# Generates 0..n-1
Digit(x) :- x in Range(10);
```

### String functions
```logica
Upper(s) :- s = ToUpper("hello");
Sub(s)   :- s = Substr("hello", 1, 3);   # "ell"
Len(n)   :- n = Length("hello");
```

### Math functions
```logica
R(x) :- x = Round(3.7);
A(x) :- x = Abs(-5);
F(x) :- x = Floor(2.9);
```

### Type conversion
```logica
AsStr(s) :- s = ToString(42);
AsNum(n) :- n = ToInt64("42");
```

---

## Lists

```logica
# List literal
MyList(l) :- l = [1, 2, 3];

# Element access
First(x) :- l = [1, 2, 3], x = l[0];

# List construction via aggregation
Names(names List= name) :- Person(name, _);
```

---

## Structs / Records

```logica
Point(p) :- p = {x: 1, y: 2};
GetX(x)  :- p = {x: 1, y: 2}, x = p.x;
```

---

## Imports & Modules

Split code across files and import predicates:

```logica
import path.to.module;
```

Or use `@` to specify a predicate from another file:

```logica
@"other_file.l":OtherPredicate(x);
```

---

## Functors (Parameterized Predicates)

Functors let you pass predicates as arguments to create reusable templates:

```logica
# Define a functor
Filtered(x) :- Source(x), x > threshold;

# Instantiate with different thresholds
BigItems = Filtered{Source: AllItems, threshold: 100};
HugeItems = Filtered{Source: AllItems, threshold: 1000};
```

---

## `distinct` Keyword

Deduplicate results:

```logica
UniqueColors(c) distinct :- Item(_, c);
```

---

## Common Patterns

### Join
```logica
EmployeeDept(name:, dept:) :-
  Employee(id, name),
  WorksIn(id, dept_id),
  Department(dept_id, dept);
```

### Self-join / Recursive
```logica
Ancestor(a, b) :- Parent(a, b);
Ancestor(a, b) :- Parent(a, mid), Ancestor(mid, b);
```

### Set difference (NOT IN)
```logica
Unassigned(emp) :- Employee(emp), ~WorksIn(emp, _);
```

### Top-N via ArgMax
```logica
BestScore(user:, score max= s) :- Score(user, s);
```

### Conditional / CASE-like
```logica
Label(x, "big")   :- Data(x), x > 100;
Label(x, "small") :- Data(x), x <= 100;
```

---

## Running Logica

```bash
# Run predicate to stdout
logica file.l run MyPredicate

# Run on specific backend
logica --use_backend sqlite file.l run MyPredicate

# Show generated SQL
logica file.l sql MyPredicate

# Run in Python
from logica import colab_logica
colab_logica.run('''
  Answer(x) :- x in Range(5);
''', 'Answer')
```

### Backends
| Backend    | Flag                    |
|------------|-------------------------|
| BigQuery   | `--use_backend bigquery` (default) |
| PostgreSQL | `--use_backend psql`    |
| SQLite     | `--use_backend sqlite`  |
| DuckDB     | `--use_backend duckdb`  |

---

## Colab / Notebook

```python
%%logica Answer
Answer(x) :- x in Range(5);
```

Install: `pip install logica`  
Playground: https://logica.dev/
