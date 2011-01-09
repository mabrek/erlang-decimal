-module(decimal_conv).

-export([float/1, number/1, string/1]).

-include_lib("eunit/include/eunit.hrl").

float({S, C, E}) ->
  math:pow(-1, S) * C * math:pow(10, E).

number(Term) when is_tuple(Term) ->
  Term;
number(Term) when is_integer(Term) andalso Term < 0 ->
  {1, erlang:abs(Term), 0};
number(Term) when is_integer(Term) ->
  {0, Term, 0};
number(Term) when is_float(Term) ->
  number(float_to_list(Term));
number(Term) when is_binary(Term) ->
  number(binary_to_list(Term));
number(Term) when is_list(Term) ->
  try
    number_without_catch(Term)
  catch
    error:function_clause ->
      {0, qNaN}
  end.

number_without_catch(String) ->
  {Sign, String2} = number_sign(String),
  number_decimal_part(string:to_lower(String2), Sign).

number_decimal_part("nan", Sign) ->
  {Sign, qNaN};
number_decimal_part("nan" ++ String, Sign) ->
  {Int, []} = number_integer(String), {Sign, qNaN, Int};
number_decimal_part("snan", Sign) ->
  {Sign, sNaN};
number_decimal_part("snan" ++ String, Sign) ->
  {Int, []} = number_integer(String), {Sign, sNaN, Int};
number_decimal_part("inf", Sign) ->
  {Sign, infinity};
number_decimal_part("infinity", Sign) ->
  {Sign, infinity};
number_decimal_part(String, Sign) ->
  {Before, String2} = number_digits(String),
  number_decimal_part(String2, Sign, Before).

number_decimal_part([$.|String], Sign, Before) ->
  {After, String2} = number_digits(String),
  number_exponent_part(String2, Sign, {Before, After});
number_decimal_part(String, Sign, Before) ->
  number_exponent_part(String, Sign, {Before, []}).

number_exponent_part([], Sign, DecimalPart) ->
  number_finish(Sign, DecimalPart, 0);
number_exponent_part([Indicator|String], Sign, DecimalPart) when Indicator =:= $e; Indicator =:= $E ->
  {Exponent, []} = number_signed_integer(String),
  number_finish(Sign, DecimalPart, Exponent).

number_finish(Sign, {Before, After}, Exponent) ->
  Coefficient = list_to_integer(string:concat(Before, After)),
  {Sign, Coefficient, Exponent - length(After)}.

number_signed_integer(String) ->
  {Sign, String2} = number_sign(String),
  {Digits, String3} = number_digits(String2),
  case Sign of
    0 -> {list_to_integer(Digits), String3};
    1 -> {-list_to_integer(Digits), String3}
  end.

number_integer(String) ->
  {Digits, String2} = number_digits(String),
  {list_to_integer(Digits), String2}.

number_digits(String) ->
  number_digits(String, []).

number_digits([H|T], Digits) when H >= $0 andalso H =< $9 ->
  number_digits(T, [H|Digits]);
number_digits(String, Digits) ->
  {lists:reverse(Digits), String}.

number_sign([$-|String]) ->
  {1, String};
number_sign([$+|String]) ->
  {0, String};
number_sign(String) ->
  {0, String}.

string({Sign, sNaN}) ->
  minus(Sign, "sNaN");
string({Sign, sNaN, Payload}) when is_integer(Payload)->
  minus(Sign, string:concat("sNaN", integer_to_list(Payload)));
string({Sign, qNaN}) ->
  minus(Sign, "NaN");
string({Sign, qNaN, Payload}) when is_integer(Payload)->
  minus(Sign, string:concat("NaN", integer_to_list(Payload)));
string({Sign, infinity}) ->
  minus(Sign, "Infinity");
string({Sign, 0, 0}) ->
  minus(Sign, "0");
string({Sign, 0, E}) when E < 0 ->
  minus(Sign, string:concat("0.", string:copies("0", -E)));
string({Sign, 0, E}) ->
  minus(Sign, string:concat("0E", plus(integer_to_list(E))));
string({Sign, C, E}) ->
  minus(Sign, punctuate(integer_to_list(C), E)).

plus(String) ->
  case hd(String) of
    $- -> String;
    _ -> [$+|String]
  end.

minus(Sign, String) ->
  case Sign of
    0 -> String;
    1 -> [$-|String]
  end.

punctuate(Digits, E) ->
  punctuate(Digits, E, E + length(Digits) - 1).

punctuate(Digits, E, AE) when E =< 0 andalso AE >= -6 ->
  case E of
    0 ->
      Digits;
    _ ->
      case AE < 0 of
        true ->
          [$0, $.|string:right(Digits, length(Digits) - AE - 1, $0)];
        false ->
          {Before, After} = lists:split(AE + 1, Digits),
          string:concat(Before, [$.|After])
      end
  end;
punctuate(Digits, _, AE) ->
  Suffix = [$E|plus(integer_to_list(AE))],
  case length(Digits) > 1 of
    true ->
      string:concat([hd(Digits), $.|tl(Digits)], Suffix);
    false ->
      string:concat(Digits, Suffix)
  end.

number_unit_test_() ->
  % cf. http://speleotrove.com/decimal/daconvs.html#reftonum
  [ ?_assertEqual({0,infinity}, number("inf"))
  , ?_assertEqual({0,infinity}, number("+inFiniTy"))
  , ?_assertEqual({1,infinity}, number("-Infinity"))
  , ?_assertEqual({0,qNaN}, number("NaN"))
  , ?_assertEqual({1,qNaN}, number("-NaN"))
  , ?_assertEqual({0,sNaN}, number("SNaN"))
  , ?_assertEqual({0,qNaN}, number("Fred"))
  , ?_assertEqual({0,0,0}, number("0"))
  , ?_assertEqual({1,0,0}, number("-0"))
  , ?_assertEqual({0,0,-2}, number("0.00"))
  , ?_assertEqual({1,0,-2}, number("-0.00"))
  , ?_assertEqual({0,0,7}, number("0E+7"))
  , ?_assertEqual({1,0,-7}, number("-0E-7"))
  , ?_assertEqual({0,123,0}, number("123"))
  , ?_assertEqual({1,123,0}, number("-123"))
  , ?_assertEqual({0,123,1}, number("1.23E3"))
  , ?_assertEqual({0,123,1}, number("1.23E+3"))
  , ?_assertEqual({0,123,6}, number("12.3E+7"))
  , ?_assertEqual({0,120,-1}, number("12.0"))
  , ?_assertEqual({0,123,-1}, number("12.3"))
  , ?_assertEqual({0,123,-5}, number("0.00123"))
  , ?_assertEqual({1,123,-14}, number("-1.23E-12"))
  , ?_assertEqual({0,12345,-5}, number("1234.5E-4"))
  ].

string_unit_test_() ->
  % cf. http://speleotrove.com/decimal/daconvs.html#reftostr
  [ ?_assertEqual("Infinity", string({0,infinity}))
  , ?_assertEqual("-Infinity", string({1,infinity}))
  , ?_assertEqual("NaN", string({0,qNaN}))
  , ?_assertEqual("NaN123", string({0,qNaN,123}))
  , ?_assertEqual("-sNaN", string({1,sNaN}))
  , ?_assertEqual("0", string({0,0,0}))
  , ?_assertEqual("-0", string({1,0,0}))
  , ?_assertEqual("0.00", string({0,0,-2}))
  , ?_assertEqual("0E+2", string({0,0,2}))
  , ?_assertEqual("123", string({0,123,0}))
  , ?_assertEqual("-123", string({1,123,0}))
  , ?_assertEqual("1.23E+3", string({0,123,1}))
  , ?_assertEqual("1.23E+5", string({0,123,3}))
  , ?_assertEqual("12.3", string({0,123,-1}))
  , ?_assertEqual("0.00123", string({0,123,-5}))
  , ?_assertEqual("1.23E-8", string({0,123,-10}))
  , ?_assertEqual("-1.23E-10", string({1,123,-12}))
  , ?_assertEqual("0.000005", string({0,5,-6}))
  , ?_assertEqual("0.0000050", string({0,50,-7}))
  , ?_assertEqual("5E-7", string({0,5,-7}))
  ].
