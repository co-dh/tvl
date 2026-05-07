; Each rule definition is its own local scope
(rule_def) @local.scope

; Variables defined in rule head (positional args)
(rule_head
  (head_arg_list
    (head_arg
      (expression
        (identifier) @local.definition))))

; Variables referenced in the rule body
(rule_body
  (goal
    (predicate_call
      (call_arg_list
        (call_arg
          (expression
            (identifier) @local.reference))))))
