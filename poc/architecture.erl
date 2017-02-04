%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(architecture).

gen_state(x86) ->
    gen_state(x86, []).

gen_state(x86, Options) 
  when is_list(Options) ->
    ok.

x86_options() ->
    
    
