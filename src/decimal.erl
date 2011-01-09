-module(decimal).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-import(decimal_conv, [number/1]).

abs(X) ->
  abs(X, []).

abs(X, Context) ->
  decimal_arith:abs(number(X), Context).

add(X, Y) ->
  add(X, Y, []).

add(X, Y, Context) ->
  decimal_arith:add(number(X), number(Y), Context).

compare(X, Y) ->
  compare(X, Y, []).

compare(X, Y, Context) ->
  decimal_arith:compare(number(X), number(Y), Context).

divide(X, Y) ->
  divide(X, Y, []).

divide(X, Y, Context) ->
  decimal_arith:divide(number(X), number(Y), Context).

divide_integer(X, Y) ->
  divide_integer(X, Y, []).

divide_integer(X, Y, Context) ->
  decimal_arith:divide_integer(number(X), number(Y), Context).

exp(X) ->
  exp(X, []).

exp(X, Context) ->
  decimal_arith:exp(number(X), Context).

format(X) ->
  decimal_conv:string(number(X)).

is_finite(X) ->
  decimal_misc:is_finite(number(X)).

is_infinite(X) ->
  decimal_misc:is_infinite(number(X)).

is_NaN(X) ->
  decimal_misc:is_NaN(number(X)).

is_qNaN(X) ->
  decimal_misc:is_qNaN(number(X)).

is_signed(X) ->
  decimal_misc:is_signed(number(X)).

is_sNaN(X) ->
  decimal_misc:is_sNaN(number(X)).

is_zero(X) ->
  decimal_misc:is_zero(number(X)).

ln(X) ->
  ln(X, []).

ln(X, Context) ->
  decimal_arith:ln(number(X), Context).

log10(X) ->
  log10(X, []).

log10(X, Context) ->
  decimal_arith:log10(number(X), Context).

max(X, Y) ->
  max(X, Y, []).

max(X, Y, Context) ->
  decimal_arith:max(number(X), number(Y), Context).

min(X, Y) ->
  min(X, Y, []).

min(X, Y, Context) ->
  decimal_arith:min(number(X), number(Y), Context).

minus(X) ->
  minus(X, []).

minus(X, Context) ->
  decimal_arith:minus(number(X), Context).

multiply(X, Y) ->
  multiply(X, Y, []).

multiply(X, Y, Context) ->
  decimal_arith:multiply(number(X), number(Y), Context).

plus(X) ->
  plus(X, []).

plus(X, Context) ->
  decimal_arith:plus(number(X), Context).

power(X, Y) ->
  power(X, Y, []).

power(X, Y, Context) ->
  decimal_arith:power(number(X), number(Y), Context).

reduce(X) ->
  reduce(X, []).

reduce(X, Context) ->
  decimal_arith:reduce(number(X), Context).

remainder(X, Y) ->
  remainder(X, Y, []).

remainder(X, Y, Context) ->
  decimal_arith:remainder(number(X), number(Y), Context).

sqrt(X) ->
  sqrt(X, []).

sqrt(X, Context) ->
  decimal_arith:sqrt(number(X), Context).

subtract(X, Y) ->
  subtract(X, Y, []).

subtract(X, Y, Context) ->
  decimal_arith:subtract(number(X), number(Y), Context).

unit_test_() ->
  % cf. http://speleotrove.com/decimal/daops.html
  [ ?_assertEqual({0,21,-1}, ?MODULE:abs("2.1"))
  , ?_assertEqual({0,100,0}, ?MODULE:abs("-100"))
  , ?_assertEqual({0,1015,-1}, ?MODULE:abs("101.5"))
  , ?_assertEqual({0,1015,-1}, ?MODULE:abs("-101.5"))
  , ?_assertEqual({0,infinity}, add("Infinity", "1"))
  , ?_assertEqual({0,qNaN}, add("NaN", "1"))
  , ?_assertEqual({0,qNaN}, add("NaN", "Infinity"))
  , ?_assertEqual({0,1900,-2}, add("12", "7.00"))
  , ?_assertEqual({0,101,2}, add("1E+2", "1E+4"))
  , ?_assertEqual({1,infinity}, subtract("1", "Infinity"))
  , ?_assertEqual({1,0,0}, subtract("-0", "0"))
  , ?_assertEqual({0,23,-2}, subtract("1.3", "1.07"))
  , ?_assertEqual({0,0,-2}, subtract("1.3", "1.30"))
  , ?_assertEqual({1,77,-2}, subtract("1.3", "2.07"))
  , ?_assertEqual(-1, compare("2.1", "3"))
  , ?_assertEqual(0, compare("2.1", "2.1"))
  , ?_assertEqual(0, compare("2.1", "2.10"))
  , ?_assertEqual(1, compare("3", "2.1"))
  , ?_assertEqual(1, compare("2.1", "-3"))
  , ?_assertEqual(-1, compare("-3", "2.1"))
  , ?_assertEqual({0,infinity}, divide("1", "0"))
  , ?_assertEqual({1,infinity}, divide("1", "-0"))
  , ?_assertEqual({1,infinity}, divide("-1", "0"))
  , ?_assertEqual({0,333333333,-9}, divide("1", "3"))
  , ?_assertEqual({0,25,-1}, divide("5", "2"))
  , ?_assertEqual({0,1,-1}, divide("1", "10"))
  , ?_assertEqual({0,1,0}, divide("12", "12"))
  , ?_assertEqual({0,400,-2}, divide("8.00", "2"))
  , ?_assertEqual({0,120,-2}, divide("2.400", "2.0"))
  , ?_assertEqual({0,10,0}, divide("1000", "100"))
  , ?_assertEqual({0,1000,0}, divide("1000", "1"))
  , ?_assertEqual({0,120,4}, divide("2.40E+6", "2"))
  , ?_assertEqual({0,333333333,0}, divide_integer("1000000000", "3"))
  , ?_assertEqual({0,0,0}, divide_integer("2", "3"))
  , ?_assertEqual({0,3,0}, divide_integer("10", "3"))
  , ?_assertEqual({0,3,0}, divide_integer("1", "0.3"))
  , ?_assertEqual({0,0,0}, exp("-Infinity"))
  , ?_assertEqual({0,367879441,-9}, exp("-1"))
  , ?_assertEqual({0,1,0}, exp("0"))
  , ?_assertEqual({0,271828183,-8}, exp("1"))
  , ?_assertEqual({0,200000000,-8}, exp("0.693147181"))
  , ?_assertEqual({0,infinity}, exp("+Infinity"))
  , ?_assertEqual({1,infinity}, ln("0"))
  , ?_assertEqual({0,0,0}, ln("1.000"))
  , ?_assertEqual({0,100000000,-8}, ln("2.71828183"))
  , ?_assertEqual({0,230258509,-8}, ln("10"))
  , ?_assertEqual({0,infinity}, ln("+Infinity"))
  , ?_assertEqual({1,infinity}, log10("0"))
  , ?_assertEqual({1,3,0}, log10("0.001"))
  , ?_assertEqual({0,0,0}, log10("1.000"))
  , ?_assertEqual({0,301029996,-9}, log10("2"))
  , ?_assertEqual({0,1,0}, log10("10"))
  , ?_assertEqual({0,184509804,-8}, log10("70"))
  , ?_assertEqual({0,infinity}, log10("+Infinity"))
  , ?_assertEqual({0,3,0}, ?MODULE:max("3", "2"))
  , ?_assertEqual({0,3,0}, ?MODULE:max("-10", "3"))
  , ?_assertEqual({0,1,0}, ?MODULE:max("1.0", "1"))
  , ?_assertEqual({0,7,0}, ?MODULE:max("7", "NaN"))
  , ?_assertEqual({0,2,0}, ?MODULE:min("3", "2"))
  , ?_assertEqual({1,10,0}, ?MODULE:min("-10", "3"))
  , ?_assertEqual({0,10,-1}, ?MODULE:min("1.0", "1"))
  , ?_assertEqual({0,7,0}, ?MODULE:min("7", "NaN"))
  , ?_assertEqual({1,13,-1}, minus("1.3"))
  , ?_assertEqual({0,13,-1}, minus("-1.3"))
  , ?_assertEqual({0,13,-1}, plus("1.3"))
  , ?_assertEqual({1,13,-1}, plus("-1.3"))
  , ?_assertEqual({1,infinity}, multiply("-1", "Infinity"))
  , ?_assertEqual({1,0,0}, multiply("-1", "0"))
  , ?_assertEqual({0,360,-2}, multiply("1.20", "3"))
  , ?_assertEqual({0,21,0}, multiply("7", "3"))
  , ?_assertEqual({0,72,-2}, multiply("0.9", "0.8"))
  , ?_assertEqual({1,0,0}, multiply("0.9", "-0"))
  , ?_assertEqual({0,428135971,3}, multiply("654321", "654321"))
  , ?_assertEqual({0,8,0}, power("2", "3"))
  , ?_assertEqual({1,8,0}, power("-2", "3"))
  , ?_assertEqual({0,125,-3}, power("2", "-3"))
  , ?_assertEqual({0,697575744,-7}, power("1.7", "8"))
  , ?_assertEqual({0,200000000,-8}, power("10", "0.301029996"))
  , ?_assertEqual({0,0,0}, power("Infinity", "-1"))
  , ?_assertEqual({0,1,0}, power("Infinity", "0"))
  , ?_assertEqual({0,infinity}, power("Infinity", "1"))
  , ?_assertEqual({1,0,0}, power("-Infinity", "-1"))
  , ?_assertEqual({0,1,0}, power("-Infinity", "0"))
  , ?_assertEqual({1,infinity}, power("-Infinity", "1"))
  , ?_assertEqual({0,infinity}, power("-Infinity", "2"))
  , ?_assertEqual({0,qNaN}, power("0", "0"))
  , ?_assertEqual({0,21,-1}, reduce("2.1"))
  , ?_assertEqual({1,2,0}, reduce("-2.0"))
  , ?_assertEqual({0,12,-1}, reduce("1.200"))
  , ?_assertEqual({1,12,1}, reduce("-120"))
  , ?_assertEqual({0,12,1}, reduce("120.00"))
  , ?_assertEqual({0,0,0}, reduce("0.00"))
  , ?_assertEqual({0,21,-1}, remainder("2.1", "3"))
  , ?_assertEqual({0,1,0}, remainder("10", "3"))
  , ?_assertEqual({1,1,0}, remainder("-10", "3"))
  , ?_assertEqual({0,2,-1}, remainder("10.2", "1"))
  , ?_assertEqual({0,1,-1}, remainder("10", "0.3"))
  , ?_assertEqual({0,10,-1}, remainder("3.6", "1.3"))
  , ?_assertEqual({0,0,0}, sqrt("0"))
  , ?_assertEqual({1,0,0}, sqrt("-0"))
  , ?_assertEqual({0,624499800,-9}, sqrt("0.39"))
  , ?_assertEqual({0,10,0}, sqrt("100"))
  , ?_assertEqual({0,1,0}, sqrt("1"))
  , ?_assertEqual({0,10,-1}, sqrt("1.0"))
  , ?_assertEqual({0,10,-1}, sqrt("1.00"))
  , ?_assertEqual({0,264575131,-8}, sqrt("7"))
  , ?_assertEqual({0,316227766,-8}, sqrt("10"))
  , ?_assertEqual(true, is_zero("0"))
  , ?_assertEqual(false, is_zero("2.50"))
  , ?_assertEqual(true, is_zero("-0E+2"))
  , ?_assertEqual(false, is_infinite("2.50"))
  , ?_assertEqual(true, is_infinite("-Infinity"))
  , ?_assertEqual(false, is_infinite("NaN"))
  , ?_assertEqual(false, is_NaN("2.50"))
  , ?_assertEqual(true, is_NaN("NaN"))
  , ?_assertEqual(true, is_NaN("-sNaN"))
  , ?_assertEqual(false, is_signed("2.50"))
  , ?_assertEqual(true, is_signed("-12"))
  , ?_assertEqual(true, is_signed("-0"))
  ].
