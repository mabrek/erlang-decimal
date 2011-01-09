{application, decimal, [
  {vsn, "0.1.0"},
  {description, "An Erlang decimal arithmetic library"},
  {modules, [
    decimal,
    decimal_arith,
    decimal_context,
    decimal_conv,
    decimal_misc,
    decimal_rounding
  ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {start_phases, []}
]}.