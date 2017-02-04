%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(test).
-compile([export_all]).

loop([], _, Accumulator) ->
    Accumulator;
loop([Head|Tail], Function, Accumulator) ->
    Accumulator2 = Function({Head, Accumulator}),
    loop(Tail, Function, Accumulator2).
