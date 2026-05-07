; Keywords
"import" @keyword
"in" @keyword
(distinct) @keyword
"true" @boolean
"false" @boolean

; Operators
(agg_op) @operator
(negation "~" @keyword.operator)
(unary_expr operator: _ @operator)
(binary_expr operator: _ @operator)
(comparison operator: _ @operator)

["+" "-" "*" "/" "%" "**" "++" "=" "!=" "<" ">" "<=" ">="] @operator
[":-"] @keyword.operator

; Predicates / functions
(predicate_name) @function
(functor_call name: (predicate_name) @function.builtin)

; Named arguments
(call_arg name: (identifier) @variable.parameter)
(head_arg name: (identifier) @variable.parameter)
(functor_arg name: (identifier) @variable.parameter)
(struct_field name: (identifier) @property)

; Variables and identifiers
(identifier) @variable

; Wildcards
(wildcard) @variable.builtin

; Literals
(string) @string
(number) @number

; Comments
(comment) @comment

; Punctuation
["(" ")" "[" "]" "{" "}"] @punctuation.bracket
["," "." ";"] @punctuation.delimiter
[":" ":-"] @punctuation.delimiter
