-module(decimal_misc).

-compile(export_all).

is_finite(X) ->
  not is_infinite(X) andalso not is_NaN(X).

is_infinite(X) ->
  element(2, X) =:= infinity.

is_NaN(X) ->
  is_qNaN(X) orelse is_sNaN(X).

is_qNaN(X) ->
  element(2, X) =:= qNaN.

is_signed(X) ->
  element(1, X) =:= 1.

is_sNaN(X) ->
  element(2, X) =:= sNaN.

is_zero(X) ->
  element(2, X) =:= 0.
