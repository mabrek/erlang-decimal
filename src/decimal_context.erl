-module(decimal_context).

-compile(export_all).

rounding(Context) ->
  proplists:get_value(rounding, Context, round_half_up).

precision(Context) ->
  proplists:get_value(precision, Context, 9).
