%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(mips).
-compile([export_all]).
-include_lib("mips.hrl").
-include_lib("eunit/include/eunit.hrl").

-type type() :: arithmetic | branch | jump | control | load
	      | store | memory_control | logical | move | shift
	      | trap | obsolete | fpu_arithmetic | fpu_branch 
	      | fpu_compare | fpu_convert | fpu_load | fpu_store
	      | fpu_move | fpu_obsolete | coproc_branch | coproc_execute
	      | coproc_load | coproc_store | coproc_move | coproc_obsolete
	      | privileged | ejtag.
-type name() :: atom().
-type value() :: iolist().
-type format() :: iolist().
-type binspec() :: {atom(), integer(), integer()}.
-type binspecs() :: [binspec(), ...].
-type operation() :: [tuple()].
-type exception() :: integer_overflow.
-type exceptions() :: [exception(), ...].
-type restriction() :: atom().
-type restrictions() :: [restriction(), ...].
-type endianess() :: big | little.
-type notes() :: [list(), ...].

-define(UNARY(Input, Action), 
	(Action)(Input)).

-define(BINARY(InputA, InputB, Action), 
	fun(InputA, InputB) ->
		(Action)(Input1, InputB) 
	end).
-define(TERNARY(InputA, InputB, InputC, Action),
       fun(InputA, InputB, InputC, Action) ->
	       (Action)(InputA, InputB, InputC) 
       end).

-record(instruction, 
	{ type              :: type()
	, name              :: name()
	, value = <<>>      :: value()
        , format = []       :: format()
	, binary = []       :: binspecs()
	, operation = []    :: operation()
	, restriction = []  :: restrictions()
	, exception = []    :: exceptions()
        , fp_exception = [] :: exceptions()
	, note = []         :: notes()
	, endianess         :: endianess()
	}).

%-record(instruction, 
%	{ name :: atom() = add
%	, value :: iolist() = <<1,0,0,0,0,0>>
%	, binary :: list() = [{special, 6}, {rs, 5}, {rt, 5}, {rd, 5}, {null, 5}, {instr, 6}]
%	, operation :: list() = [{op,1,'+',{var,1,'Rs'},{var,1,'Rt'}}]
%	, restriction :: list() = []
%	, exception :: list() = [integer_overflow]
%	, note :: list() = []
%	}).

-define(TEST(X), merl:var(X)).
-define(VALUE(V, S), <<V:S>>).

instruction() ->
    [#instruction{ type = fpu_arithmetic
		 , name = abs
		 , value = ?VALUE(32,6)
%		 , value = <<0:1,0:1,0:1,1:1,0:1,1:1>>
		 , binary = [{cop1, 6, 17}, {fmt, 5, '_'}, {null, 5, 0}
			    ,{fs, 5, '_'}, {fd, 5, '_'}, {instr, 6, '_'}]
		 , operation = [{call,1,{atom,1,abs},[{var,1,'Fs'}]}]
		 , restriction = []
		 , exception = [coprocessor_unusable,
				reversed_instruction]
		 }
    ,#instruction{ type = arithmetic
		 , name = add
		 , value = ?VALUE(32,6)
		 , binary = [{special, 6, '_'}, {rs, 5, '_'}, {rt, 5, '_'}
			    ,{rd, 5, '_'}, {null, 5, 0}, {instr, 6, '_'}]
		 , operation = [{op,1,'+',{var,1,'Rs'},{var,1,'Rt'}}]
		 , exception = [integer_overflow]
		 , fp_exception = []
		 }
    ].

instruction_by_name() ->
    [ {Instr#instruction.name, Instr} || Instr <- instruction() ].
instruction_by_value() ->
    [ {Instr#instruction.value, Instr} || Instr <- instruction() ].
instruction_by_exception() ->
    [ {Instr#instruction.exception, Instr} || Instr <- instruction() ].


t() ->
    ?UNARY(Rs, (fun(X) -> X end)).
