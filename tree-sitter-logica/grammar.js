// Tree-sitter grammar for Google's Logica language.
// Logica is a logic programming language that compiles to SQL.
// https://logica.dev

const PREC = {
  compare: 1,
  add: 2,
  mul: 3,
  power: 4,
  unary: 5,
  call: 6,
};

module.exports = grammar({
  name: 'logica',

  extras: $ => [
    /\s+/,
    $.comment,
  ],

  word: $ => $.identifier,

  inline: $ => [$.primary],

  conflicts: $ => [
    [$.goal, $.expression],
    [$.comparison, $.expression],
  ],

  rules: {
    program: $ => repeat(
      choice(
        $.rule_def,
        $.import_stmt,
        $.functor_def,
      )
    ),

    // import path.to.module;
    import_stmt: $ => seq(
      'import',
      $.dotted_name,
      ';',
    ),

    // Name = Functor{Key: Val, ...};
    functor_def: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $.functor_call),
      optional(';'),
    ),

    // Fact: Head(...).
    // Rule: Head(...) :- Body;
    rule_def: $ => seq(
      field('head', $.rule_head),
      choice(
        '.', // fact
        seq(':-', field('body', $.rule_body), ';'),
      ),
    ),

    rule_head: $ => seq(
      field('predicate', $.predicate_name),
      '(',
      optional(field('args', $.head_arg_list)),
      ')',
      optional(seq(
        field('agg_op', $.agg_op),
        field('agg_value', $.expression),
      )),
      optional($.distinct),
    ),

    head_arg_list: $ => seq(
      $.head_arg,
      repeat(seq(',', $.head_arg)),
      optional(','),
    ),

    // Head args: named (col: expr), bare named (col:), aggregation (col += expr), or expression.
    // Higher prec for named/agg forms so `col: expr` and `col += expr` are preferred over bare.
    head_arg: $ => choice(
      prec(2, seq(field('name', $.identifier), ':', field('value', $.expression))),
      prec(2, seq(field('name', $.identifier), field('agg_op', $.agg_op), field('agg_value', $.expression))),
      seq(field('name', $.identifier), ':'),
      $.expression,
    ),

    // Aggregation operators used in the rule head
    agg_op: $ => token(choice('+=', 'max=', 'min=', 'List=', '?=')),

    rule_body: $ => seq(
      $.goal,
      repeat(seq(',', $.goal)),
    ),

    // Body goals
    goal: $ => choice(
      $.negation,
      $.in_expr,
      $.comparison,
      $.predicate_call,
    ),

    // ~Predicate(args) or ~(comparison)
    negation: $ => seq('~', choice($.predicate_call, $.comparison)),

    // expr op expr (= is used for unification/assignment in Logica)
    comparison: $ => prec.left(PREC.compare, seq(
      field('left', $.expression),
      field('operator', $.compare_op),
      field('right', $.expression),
    )),

    compare_op: $ => choice('=', '!=', '<', '>', '<=', '>='),

    // x in Collection
    in_expr: $ => seq(
      field('element', $.expression),
      'in',
      field('collection', $.expression),
    ),

    // Pred(arg1, arg2, ...)
    predicate_call: $ => prec(PREC.call, seq(
      field('name', $.predicate_name),
      '(',
      optional(field('args', $.call_arg_list)),
      ')',
    )),

    call_arg_list: $ => seq(
      $.call_arg,
      repeat(seq(',', $.call_arg)),
      optional(','),
    ),

    call_arg: $ => choice(
      prec(1, seq(field('name', $.identifier), ':', field('value', $.expression))),
      seq(field('name', $.identifier), ':'),
      $.expression,
    ),

    // Pred{Key: Val, ...}
    functor_call: $ => seq(
      field('name', $.predicate_name),
      '{',
      optional(field('args', $.functor_arg_list)),
      '}',
    ),

    functor_arg_list: $ => seq(
      $.functor_arg,
      repeat(seq(',', $.functor_arg)),
      optional(','),
    ),

    functor_arg: $ => seq(
      field('name', $.identifier),
      ':',
      field('value', $.expression),
    ),

    // Expressions (arithmetic / string / literals / calls)
    expression: $ => choice(
      $.binary_expr,
      $.unary_expr,
      $.primary,
    ),

    binary_expr: $ => choice(
      prec.left(PREC.power, seq(field('left', $.expression), field('operator', '**'),  field('right', $.expression))),
      prec.left(PREC.mul,   seq(field('left', $.expression), field('operator', '*'),   field('right', $.expression))),
      prec.left(PREC.mul,   seq(field('left', $.expression), field('operator', '/'),   field('right', $.expression))),
      prec.left(PREC.mul,   seq(field('left', $.expression), field('operator', '%'),   field('right', $.expression))),
      prec.left(PREC.add,   seq(field('left', $.expression), field('operator', '+'),   field('right', $.expression))),
      prec.left(PREC.add,   seq(field('left', $.expression), field('operator', '-'),   field('right', $.expression))),
      prec.left(PREC.add,   seq(field('left', $.expression), field('operator', '++'),  field('right', $.expression))),
    ),

    unary_expr: $ => prec(PREC.unary, seq(
      field('operator', '-'),
      field('operand', $.expression),
    )),

    // Inlined — does not appear as a named node in the tree
    primary: $ => choice(
      $.predicate_call,
      $.functor_call,
      $.list,
      $.struct,
      $.number,
      $.string,
      $.bool,
      $.wildcard,
      $.identifier,
      seq('(', $.expression, ')'),
    ),

    list: $ => seq(
      '[',
      optional(seq(
        $.expression,
        repeat(seq(',', $.expression)),
        optional(','),
      )),
      ']',
    ),

    struct: $ => seq(
      '{',
      optional(seq(
        $.struct_field,
        repeat(seq(',', $.struct_field)),
        optional(','),
      )),
      '}',
    ),

    struct_field: $ => seq(
      field('name', $.identifier),
      ':',
      field('value', $.expression),
    ),

    dotted_name: $ => seq(
      $.identifier,
      repeat(seq('.', $.identifier)),
    ),

    // predicate_name is a named node wrapping an identifier, for highlighting
    predicate_name: $ => $.identifier,

    identifier: $ => /[A-Za-z_][A-Za-z0-9_]*/,

    wildcard: $ => '_',

    number: $ => token(choice(
      /\d+\.\d*([eE][+-]?\d+)?/,
      /\d+[eE][+-]?\d+/,
      /\d+/,
    )),

    string: $ => token(seq(
      '"',
      repeat(choice(/[^"\\]+/, /\\./)),
      '"',
    )),

    distinct: $ => 'distinct',

    bool: $ => choice('true', 'false'),

    comment: $ => token(seq('#', /.*/)),
  },
});
