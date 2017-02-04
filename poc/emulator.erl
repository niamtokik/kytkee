%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(emulator).
-behaviour(gen_fsm).
-compile([export_all]).
-include_lib("mips.hrl").
% -export([]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start() ->
    start(?MODULE, [], []).
start(Module, Args, Options) ->
    start({local, Module}, Module, Args, Options).
start(FsmName, Module, Args, Options) ->
    gen_fsm:start(FsmName, Module, Args, Options).

start_link() ->
    ok.
start_link(Module, Args, Options) ->
    ok.
start_link(FsmName, Module, Args, Options) ->
    ok.

stop() ->
    exit(whereis(emulator), 'KILL').
stop(FsmRef) ->
    ok.
stop(FsmRef, Reason, Timeout) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(mips) ->
    DataStructure = mips:init(),
    {ok, initialization, DataStructure};
init(Args) ->
    init(mips).
    

handle_event({data, Data}, StateName, StateData) ->
    {next_state, data};
handle_event({next_state, State}, StateName, StateData) ->
    {next_state, State, StateData};
handle_event({instruction, Instruction}, StateName, StateData) ->
    {next_state, nothing, StateData};
handle_event(Event, StateName, StateData) ->
    io:format("received: ~p~n", [Event]),
    {next_state, nothing, StateData}.
handle_info(Info, StateName, StateData) ->
    io:format("state: ~p, ~p~n", [StateName, StateData]),
    ok.

terminate(Reason, StateName, StateData) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
initialization(Event, StateData) ->
    io:format("received: ~p, ~p~n", [Event, StateData]),
    {next_state, instruction, StateData}.

instruction({instruction, {add, "$t0", Left, Right}}, StateData) ->
    io:format("instruction: ~p~n", [StateData]),
    Result = Left + Right,
    {next_state, instruction, StateData#register{r8 = <<Result:32>>}};
instruction(_, StateData) ->
    {next_state, instruction, StateData}.


nothing({next_state, State}, StateData) ->
    io:format("nothing: ~p~n", [StateData]),
    {next_state, State, StateData};
nothing(Event, StateData) ->
    io:format("nothing: ~p~n", [StateData]),
    {next_state, nothing, StateData}.

ending(Event, StateData) ->
    {next_state, beginning, "ended"}.

