-module(decimal_arith).

-compile(export_all).

-define(is_zero(X), (element(2, X) =:= 0)).

-define(is_signed(X), (element(1, X) =:= 1)).

-define(is_infinite(X), (element(2, X) =:= infinity)).

-define(is_qNaN(X), (element(2, X) =:= qNaN)).

-define(is_sNaN(X), (element(2, X) =:= sNaN)).

-define(is_NaN(X), (?is_qNaN(X) orelse ?is_sNaN(X))).

abs(X, _Context) when ?is_signed(X) ->
  setelement(1, X, 0);
abs(X, _Context) ->
  X.

add(X, Y, _Context) when ?is_NaN(X) orelse ?is_NaN(Y) ->
  {0, qNaN}; % TODO: copy Sign and Diagnostic info
add(X, _, _Context) when ?is_infinite(X) ->
  X;
add(_, Y, _Context) when ?is_infinite(Y) ->
  Y;
add(X, Y, Context) ->
  C = add_coefficient(X, Y),
  Z = {add_sign(X, Y, C, Context), C, erlang:min(exponent(X), exponent(Y))},
  decimal_rounding:apply(Z, Context).

add_coefficient({XS, XC, XE}, {YS, YC, YE}) ->
  add_coefficient(add_coefficient_align(XC, XE, YC, YE), XS, YS).

add_coefficient({XCA, YCA}, XS, YS) ->
  case XS =:= YS of
    true ->
      XCA + YCA;
    false ->
      erlang:max(XCA, YCA) - erlang:min(XCA, YCA)
  end.

add_coefficient_align(XC, E, YC, E) ->
  {XC, YC};
add_coefficient_align(XC, XE, YC, YE) when XE > YE ->
  {XC * integer_pow(10, XE - YE), YC};
add_coefficient_align(XC, XE, YC, YE) ->
  {XC, YC * integer_pow(10, YE - XE)}.

add_sign(X, Y, C, Context) ->
  case C =:= 0 of
    true ->
      BothNegative = (?is_signed(X) andalso ?is_signed(Y)),
      SignsDifferent = (?is_signed(X) =/= ?is_signed(Y)),
      Rounding = decimal_context:rounding(Context),
      case BothNegative orelse (SignsDifferent andalso Rounding =:= round_floor) of
        true -> 1;
        false -> 0
      end;
    false ->
      % Note: cannot use compare/2 here to avoid cyclic dependency
      % (compare/2 depends on subtract/2 which depends on add/2).
      AbsValueX = erlang:abs(decimal_conv:float(X)),
      AbsValueY = erlang:abs(decimal_conv:float(Y)),
      element(1, case AbsValueX > AbsValueY of true -> X; false -> Y end)
  end.

subtract(X, Y, Context) ->
  add(X, setelement(1, Y, case element(1, Y) of 0 -> 1; 1 -> 0 end), Context).

compare(X, Y, Context) ->
  % TODO: NaN
  % TODO: Infinity
  case ?is_signed(X) =/= ?is_signed(Y) of
    true ->
      compare_subtract(comparison_value(X), comparison_value(Y), Context);
    false ->
      compare_subtract(X, Y, Context)
  end.

compare_subtract(X, Y, Context) ->
  case subtract(X, Y, Context) of
    N when ?is_zero(N) -> 0;
    N when ?is_signed(N) -> -1;
    _ -> 1
  end.

comparison_value(X) when ?is_zero(X) ->
  {0, 0, 0};
comparison_value(X) when ?is_signed(X) ->
  {1, 1, 0};
comparison_value(_) ->
  {0, 1, 0}.

divide(X, Y, _Context) when ?is_zero(X) andalso ?is_zero(Y) ->
  {0, qNaN}; % TODO: Division undefined condition is raised
divide(X, Y, _Context) when ?is_zero(Y) ->
  {sign_xor(X, Y), infinity};
divide(X, Y, Context) ->
  divide(X, Y, 0, Context).

divide(X={_, XC, _}, Y={_, YC, _}, Adjust, Context) ->
  case ?is_zero(X) of
    true ->
      divide(X, Y, Adjust, 0, Context);
    false ->
      Precision = decimal_context:precision(Context),
      {XC2, YC2, Adjust2} = divide_adjust(XC, YC, Adjust),
      {C, Adjust3, _Remainder} = divide_calculate(XC2, YC2, 0, Adjust2, Precision),
      divide(X, Y, Adjust3, C, Context)
  end.

divide(X, Y, Adjust, C, Context) ->
  Z = {sign_xor(X, Y), C, exponent(X) - (exponent(Y) + Adjust)},
  decimal_rounding:apply(Z, Context).

divide_adjust(XC, YC, Adjust) when XC < YC ->
  divide_adjust(XC * 10, YC, Adjust + 1);
divide_adjust(XC, YC, Adjust) when XC >= 10 * YC ->
  divide_adjust(XC, YC * 10, Adjust - 1);
divide_adjust(XC, YC, Adjust) ->
  {XC, YC, Adjust}.

divide_calculate(XC, YC, ZC, Adjust, Precision) ->
  case YC =< XC of
    true ->
      divide_calculate(XC - YC, YC, ZC + 1, Adjust, Precision);
    false ->
      case divide_complete(XC, ZC, Adjust, Precision) of
        true ->
          {ZC, Adjust, XC};
        false ->
          divide_calculate(XC * 10, YC, ZC * 10, Adjust + 1, Precision)
      end
  end.

divide_complete(XC, ZC, Adjust, Precision) when is_integer(Precision) ->
  (XC =:= 0 andalso Adjust >= 0) orelse (length(integer_to_list(ZC)) =:= Precision);
divide_complete(_, _, Adjust, {integer, Precision}) ->
  Precision =:= Adjust.

divide_integer(X, Y, Context) ->
  case compare(?MODULE:abs(X, Context), ?MODULE:abs(Y, Context), Context) =:= -1 of
    true ->
      zero();
    false ->
      % TODO: fail if more digits than precision
      Precision = {integer, exponent(X) - exponent(Y)},
      {XC, YC, Adjust} = divide_adjust(coefficient(X), coefficient(Y), 0),
      {ZC, _Adjust2, _Remainder} = divide_calculate(XC, YC, 0, Adjust, Precision),
      % Precision
      {sign(X), ZC, 0}
  end.

exp(X, _Context) when ?is_infinite(X) andalso ?is_signed(X) ->
  zero();
exp(X, _Context) when ?is_infinite(X) ->
  {0, infinity};
exp(X, _Context) when ?is_zero(X) ->
  one();
exp(X, Context) ->
  Z = decimal_conv:number(math:exp(decimal_conv:float(X))),
  decimal_rounding:apply(Z, [{rounding, round_half_even}|Context]).

ln(X, _Context) when ?is_zero(X) ->
  {1, infinity};
ln(X, _Context) when ?is_infinite(X) andalso not ?is_signed(X) ->
  {0, infinity};
ln(X, Context) ->
  case compare(X, one(), Context) of
    0 ->
      zero();
    _ ->
      Z = decimal_conv:number(math:log(decimal_conv:float(X))),
      decimal_rounding:apply(Z, [{rounding, round_half_even}|Context])
  end.

log10(X, _Context) when ?is_zero(X) ->
  {1, infinity};
log10(X, _Context) when ?is_infinite(X) andalso not ?is_signed(X) ->
  {0, infinity};
log10(X, Context) ->
  case log10_power(X) of
    true ->
      decimal_conv:number(round(math:log10(decimal_conv:float(X))));
    false ->
      Z = decimal_conv:number(math:log10(decimal_conv:float(X))),
      decimal_rounding:apply(Z, [{rounding, round_half_even}|Context])
  end.

log10_power({_, C, _}) ->
  log10_power(integer_to_list(C));
log10_power([$1|Digits]) ->
  lists:all(fun(Digit) -> Digit =:= $0 end, Digits);
log10_power(_) ->
  false.

% TODO: If either operand is a NaN then the general rules apply
max(X, Y, _Context) when ?is_qNaN(X) ->
  Y;
max(X, Y, _Context) when ?is_qNaN(Y) ->
  X;
max(X, Y, Context) ->
  case compare(X, Y, Context) of
    1 -> X;
    -1 -> Y;
    0 -> max_equal(X, Y)
  end.

max_equal(X, X) ->
  X;
max_equal(X, Y) when ?is_signed(X) andalso ?is_signed(Y) ->
  case exponent(X) < exponent(Y) of true -> X; false -> Y end;
max_equal(X, Y) when (not ?is_signed(X)) andalso (not ?is_signed(Y)) ->
  case exponent(X) > exponent(Y) of true -> X; false -> Y end;
max_equal(X, Y) when (not ?is_signed(X)) andalso ?is_signed(Y) ->
  X;
max_equal(X, Y) when (not ?is_signed(Y)) andalso ?is_signed(X) ->
  Y.

% TODO: If either operand is a NaN then the general rules apply
min(X, Y, _Context) when ?is_qNaN(X) ->
  Y;
min(X, Y, _Context) when ?is_qNaN(Y) ->
  X;
min(X, Y, Context) ->
  case compare(X, Y, Context) of
    1 -> Y;
    -1 -> X;
    0 -> min_equal(X, Y)
  end.

min_equal(X, X) ->
  X;
min_equal(X, Y) when ?is_signed(X) andalso ?is_signed(Y) ->
  case exponent(X) > exponent(Y) of true -> X; false -> Y end;
min_equal(X, Y) when (not ?is_signed(X)) andalso (not ?is_signed(Y)) ->
  case exponent(X) < exponent(Y) of true -> X; false -> Y end;
min_equal(X, Y) when (not ?is_signed(X)) andalso ?is_signed(Y) ->
  Y;
min_equal(X, Y) when (not ?is_signed(Y)) andalso ?is_signed(X) ->
  X.

minus(X, Context) ->
  subtract(zero(), X, Context).

multiply(X, Y, _Context) when ?is_zero(X) orelse ?is_zero(Y) ->
  {sign_xor(X, Y), 0, 0};
multiply(X, Y, _Context) when ?is_infinite(X) orelse ?is_infinite(Y) ->
  {sign_xor(X, Y), infinity};
multiply(X={_, XC, XE}, Y={_, YC, YE}, Context) ->
  Z = {sign_xor(X, Y), XC * YC, XE + YE},
  decimal_rounding:apply(Z, Context).

plus(X, Context) ->
  add(zero(), X, Context).

power(X, Y, _Context) when ?is_zero(X) andalso ?is_zero(Y) ->
  {power_sign(X, Y), qNaN};
power(X, Y, _Context) when ?is_infinite(X) andalso ?is_zero(Y) ->
  {power_sign(X, Y), 1, 0};
power(X, Y, _Context) when ?is_infinite(X) andalso ?is_signed(Y) ->
  {power_sign(X, Y), 0, 0};
power(X, Y, _Context) when ?is_infinite(X) ->
  {power_sign(X, Y), infinity};
power(X, Y, _Context) when ?is_zero(X) andalso ?is_signed(Y) ->
  {power_sign(X, Y), infinity};
power(X, Y, _Context) when ?is_zero(X) ->
  {power_sign(X, Y), 0, 0};
power(X, Y, _Context) when ?is_zero(Y) ->
  {power_sign(X, Y), 1, 0};
power(X, {1, C, 0}, Context) ->
  Reciprocal = divide(one(), X, Context),
  power_multiply(C, Reciprocal, Reciprocal, Context);
power(X, {0, C, 0}, Context) ->
  power_multiply(C, X, X, Context);
power(X, Y, Context) ->
  Z = decimal_conv:number(math:pow(decimal_conv:float(X), decimal_conv:float(Y))),
  decimal_rounding:apply(Z, Context).

power_multiply(1, _, Acc, _Context) ->
  Acc;
power_multiply(N, X, Acc, Context) ->
  power_multiply(N - 1, X, multiply(X, Acc, Context), Context).

power_sign(X, {_, N, 0}) when ?is_signed(X) ->
  N rem 2;
power_sign(_, _) ->
  0.

reduce({S, C, _}, _Context) when C =:= 0 ->
  {S, 0, 0};
reduce(X={S, C, E}, Context) ->
  case C rem 10 =:= 0 of
    true ->
      reduce({S, C div 10, E + 1}, Context);
    false ->
      X
  end.

remainder(X, Y, Context) ->
  case compare(?MODULE:abs(X, Context), ?MODULE:abs(Y, Context), Context) of
    1 ->
      subtract(X, multiply(Y, divide_integer(X, Y, Context), Context), Context);
    _ ->
      X
  end.

sqrt(X, _Context) when ?is_zero(X) ->
  X;
sqrt(X, _Context) when ?is_signed(X) ->
  {error, invalid_operation};
sqrt(X, Context) ->
  Precision = decimal_context:precision(Context) + 1,
  CoefficientDigits = length(integer_to_list(coefficient(X))),
  case exponent(X) band 1 of
    0 ->
      Shift = Precision - ((CoefficientDigits + 1) bsr 1),
      sqrt(X, Context, Shift, coefficient(X));
    _ ->
      Shift = Precision - ((CoefficientDigits bsr 1) + 1),
      sqrt(X, Context, Shift, coefficient(X) * 10)
  end.

sqrt(X, Context, Shift, C) ->
  case Shift >= 0 of
    true ->
      sqrt(X, Context, Shift, C * trunc(math:pow(10, Shift bsl 1)), true);
    false ->
      Operand = trunc(math:pow(10, (- Shift) bsl 1)),
      sqrt(X, Context, Shift, C div Operand, C rem Operand =:= 0)
  end.

sqrt(X, Context, Shift, C, Exact) ->
  E = exponent(X) bsr 1,
  N = sqrt_loop(C, trunc(math:pow(10, decimal_context:precision(Context) + 1))),
  case Exact and (N * N =:= C) of
    true ->
      case Shift >= 0 of
        true ->
          sqrt_round(Context, {0, N div trunc(math:pow(10, Shift)), E});
        false ->
          sqrt_round(Context, {0, N * trunc(math:pow(10, -Shift)), E})
      end;
    false ->
      sqrt_round(Context, {0, N, E - Shift})
  end.

sqrt_round(Context, X) ->
  decimal_rounding:apply(round_half_even, decimal_context:precision(Context), X).

sqrt_loop(C, N) ->
  Q = C div N,
  case N =< Q of
    true ->
      N;
    false ->
      sqrt_loop(C, (N + Q) bsr 1)
  end.

-compile({inline, [{zero, 0}, {one, 0}, {sign, 1}, {coefficient, 1}, {exponent, 1}]}).

zero() ->
  {0, 0, 0}.

one() ->
  {0, 1, 0}.

sign(X) ->
  element(1, X).

coefficient(X) ->
  element(2, X).

exponent(X) ->
  element(3, X).

sign_xor(X, Y) ->
  XS = case element(1, X) of 0 -> false; 1 -> true end,
  YS = case element(1, Y) of 0 -> false; 1 -> true end,
  case XS xor YS of
    false -> 0;
    true -> 1
  end.

integer_pow(X, Y) when Y >= 0 ->
  integer_pow(X, Y, 1).

integer_pow(X, Y, Z) ->
  case Y =:= 0 of
    true -> Z;
    false -> integer_pow(X, Y - 1, Z * X)
  end.
