An Erlang decimal arithmetic library.

Quick start:

  $ make
  ...
  $ erl -pa ebin
  ...
  1> decimal:add("1.3", "1.07").
  {0,237,-2}
  2> decimal:format({0,237,-2}).
  "2.37"
  3> decimal:format(decimal:subtract("1.3", "1.07")).
  "0.23"
  4> decimal:format(decimal:sqrt("10")).
  "3.16227766"
  5> decimal:format(decimal:sqrt("10", [{precision, 3}])).
  "3.16"


The "decimal" module defines the complete public interface.

There are EUnit tests in the "decimal", "decimal_conv", and "decimal_rounding" modules.

The design of this library is based on the "General Decimal Arithmetic"
specification (http://speleotrove.com/decimal/decarith.html).
