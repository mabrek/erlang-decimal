-module(decimal_rounding).

-export([apply/2, apply/3, digits_increment/1]).

-include_lib("eunit/include/eunit.hrl").

-define(HALF, $5).

apply(X, Context) ->
  ?MODULE:apply(decimal_context:rounding(Context), decimal_context:precision(Context), X).

apply(Algorithm, Precision, X={S, C, E}) ->
  Digits = integer_to_list(C),
  Length = length(Digits),
  case Length > Precision of
    true ->
      {SignificantDigits, RemainingDigits} = lists:split(Precision, Digits),
      case apply_increment(Algorithm, S, SignificantDigits, RemainingDigits) of
        true ->
          Y = {S, digits_increment(SignificantDigits), E + length(RemainingDigits)},
          ?MODULE:apply(Algorithm, Precision, Y);
        false ->
          {S, list_to_integer(SignificantDigits), E + length(RemainingDigits)}
      end;
    false ->
      X
  end.

digits_increment(Digits) ->
  digits_increment(lists:reverse(Digits), []).

digits_increment([], Acc) ->
  list_to_integer([$1|Acc]);
digits_increment([$9|T], Acc) ->
  digits_increment(T, [$0|Acc]);
digits_increment([H|T], Acc) ->
  list_to_integer(lists:reverse(T, [H + 1|Acc])).

apply_increment(round_down, _, _, _) ->
  false;
apply_increment(round_half_up, _, _, [Digit|_]) ->
  Digit >= ?HALF;
apply_increment(round_half_even, _, SignificantDigits, [Digit|_]) ->
  Digit > ?HALF orelse (Digit =:= ?HALF andalso lists:last(SignificantDigits) rem 2 =:= 1);
apply_increment(round_ceiling, S, _, RemainingDigits) ->
  S =:= 0 andalso any_nonzero(RemainingDigits);
apply_increment(round_floor, S, _, RemainingDigits) ->
  S =:= 1 andalso any_nonzero(RemainingDigits).

any_nonzero(Digits) ->
  lists:any(fun(Digit) -> Digit =/= $0 end, Digits).

digits_increment_test_() ->
  [ ?_assertEqual(235, digits_increment("234"))
  , ?_assertEqual(236, digits_increment("235"))
  , ?_assertEqual(240, digits_increment("239"))
  , ?_assertEqual(1000, digits_increment("999"))
  ].

unit_test_() ->
  [ ?_assertEqual({0,10,-1}, ?MODULE:apply(round_down, 2, {0,102,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_down, 2, {0,104,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_down, 2, {0,105,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_down, 2, {0,106,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_down, 2, {0,108,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_half_up, 2, {0,102,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_half_up, 2, {0,104,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_up, 2, {0,105,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_up, 2, {0,106,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_up, 2, {0,108,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_half_even, 2, {0,102,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_half_even, 2, {0,104,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_half_even, 2, {0,105,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_even, 2, {0,106,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_even, 2, {0,108,-2}))
  , ?_assertEqual({0,67,-1}, ?MODULE:apply(round_half_even, 2, {0,666,-2}))
  , ?_assertEqual({0,10,+0}, ?MODULE:apply(round_half_even, 2, {0,999,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_even, 2, {0,112,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_half_even, 2, {0,114,-2}))
  , ?_assertEqual({0,12,-1}, ?MODULE:apply(round_half_even, 2, {0,115,-2}))
  , ?_assertEqual({0,12,-1}, ?MODULE:apply(round_half_even, 2, {0,116,-2}))
  , ?_assertEqual({0,12,-1}, ?MODULE:apply(round_half_even, 2, {0,118,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_ceiling, 2, {0,102,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_ceiling, 2, {0,104,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_ceiling, 2, {0,105,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_ceiling, 2, {0,106,-2}))
  , ?_assertEqual({0,11,-1}, ?MODULE:apply(round_ceiling, 2, {0,108,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_floor, 2, {0,102,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_floor, 2, {0,104,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_floor, 2, {0,105,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_floor, 2, {0,106,-2}))
  , ?_assertEqual({0,10,-1}, ?MODULE:apply(round_floor, 2, {0,108,-2}))
  ].
