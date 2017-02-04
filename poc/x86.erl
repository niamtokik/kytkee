-module(x86).
-compile([export_all]).

-define(BYTE,    8).
-define(WORD,   16).
-define(DOUBLE, 32).
-define(LONG,   64).

opcode(<<>>) -> 
    ok;
opcode(<<OPCODE:64/bitstring, Rest/bitstring>>) ->
    IO = <<OPCODE:64/bitstring, Rest/bitstring>>,
    PRINT = fun(X) -> io:format("~p~n", [X]) end,
    io:format("~p ", [OPCODE]),
    case OPCODE of
	<<16#9B, _>> -> opcode(wait_dec(IO));
	<<16#0F, 16#09, _>> -> opcode(wbinvd_dec(IO));
	<<16#86, _>> -> opcode(xchg_dec(IO));
	<<16#87, _>> -> opcode(xchg_dec(IO));
	<<16#91, _>> -> opcode(xchg_dec(IO));
	16#92 -> opcode(xchg_dec(IO));
        16#93 -> opcode(xchg_dec(IO));
        16#94 -> opcode(xchg_dec(IO));
        16#95 -> opcode(xchg_dec(IO));
        16#96 -> opcode(xchg_dec(IO));
        16#97 -> opcode(xchg_dec(IO));
        16#D7 -> opcode(xlat_dec(IO));
        <<16#30, _>> -> opcode(xor_dec(IO));
        <<16#31, _>> -> opcode(xor_dec(IO));
        <<16#32, _>> -> opcode(xor_dec(IO));
        <<16#33, _>> -> opcode(xor_dec(IO));
        <<16#34, _>> -> opcode(xor_dec(IO));
        <<16#35, _>> -> opcode(xor_dec(IO));
        <<16#80, _>> -> opcode(xor_dec(IO));
        <<16#81, _>> -> opcode(xor_dec(IO));
	<<16#83, _>> -> opcode(xor_dec(IO));
	<<16#E8, _>> -> opcode(call_dec(IO, PRINT(call)));
	<<16#FF, _>> -> opcode(call_dec(IO, PRINT(call)));
	<<16#91, _>> -> opcode(call_dec(IO, PRINT(call)));
	<<16#00, _>> -> opcode(add_dec(IO, PRINT(add)));
	<<16#01, _>> -> opcode(add_dec(IO, PRINT(add)));
	<<16#02, _>> -> opcode(add_dec(IO, PRINT(add)));
	<<16#03, _>> -> opcode(add_dec(IO, PRINT(add)));
	<<16#04, _>> -> opcode(add_dec(IO, PRINT(add)));
	<<16#05, _>> -> opcode(add_dec(IO, PRINT(add)));
	_ -> opcode(Rest)
    end.

add_dec(<<16#00, _, _, _, Rest/bitstring>>) -> Rest;
add_dec(<<16#01, _, _, _, Rest/bitstring>>) -> Rest;
add_dec(<<16#02, _, _, _, Rest/bitstring>>) -> Rest;
add_dec(<<16#03, _, _, _, Rest/bitstring>>) -> Rest;
add_dec(<<16#04, _, Rest/bitstring>>) -> Rest;
add_dec(<<16#05, _, _, Rest/bitstring>>) -> Rest.

add_dec(Bitstring, Fun) ->
    Fun(),
    add_dec(Bitstring).
    

call_dec(<<16#E8, _, _, Rest/bitstring>>)  ->
    Rest;
call_dec(<<16#FF, _, _, _, Rest/bitstring>>) ->
    Rest;
call_dec(<<16#91, _, _, SL, SH, Rest/bitstring>>) ->
    Rest.

call_dec(Bitstring, Fun) when is_function(Fun) ->
    Fun(),
    call_dec(Bitstring).


%%--------------------------------------------------------------------
%% wait_dec
%%--------------------------------------------------------------------
-spec wait_dec(iolist()) -> iolist().
wait_dec(<<16#9B, Rest/bitstring>>) ->
    Rest.

%%--------------------------------------------------------------------
%% wbinvd_dec
%%--------------------------------------------------------------------
-spec wbinvd_dec(iolist()) -> iolist().
wbinvd_dec(<<16#0F, 16#09, 
	 Rest/bitstring>>) ->
    Rest.

%%--------------------------------------------------------------------
%% xchg_dec
%%--------------------------------------------------------------------
-spec xchg_dec(iolist()) -> iolist().
xchg_dec(<<16#86, MR, D0, D1, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#87, MR, D0, D1, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#91, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#92, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#93, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#94, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#95, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#96, 
       Rest/bitstring>>) -> 
    Rest;
xchg_dec(<<16#97, 
       Rest/bitstring>>) -> 
    Rest.

%%--------------------------------------------------------------------
%% xlat_dec
%%--------------------------------------------------------------------
-spec xlat_dec(iolist()) -> iolist().
xlat_dec(<<16#D7,
       Rest/bitstring>>) -> 
    Rest.

%%--------------------------------------------------------------------
%% xor_dec
%%--------------------------------------------------------------------
-spec xor_dec(iolist()) -> iolist().
xor_dec(<<16#30, S, MR, D0, D1, 
	Rest/bitstring>>) -> 
    Rest;
xor_dec(<<16#31, S, MR, DO, D1, 
	Rest/bitstring>>) -> 
    Rest;
xor_dec(<<16#32, MR, D0, D1, 
	Rest/bitstring>>) ->
    Rest;
xor_dec(<<16#33, MR, D0, D1, 
	Rest/bitstring>>) ->
    Rest;
xor_dec(<<16#34, I0:8, 
      Rest/bitstring>>) ->
    Rest;
xor_dec(<<16#35, I0:8, I1:8, 
      Rest/bitstring>>) ->
    Rest;
xor_dec(<<16#80, 1:2, DIG:3, RM:3, D0, D1, I0, 
      Rest/bitstring>>) -> 
    Rest;
xor_dec(<<16#81, 1:2, DIG:3, RM:3, D0, D1, I0, I1, 
      Rest/bitstring>>) -> 
    Rest;
xor_dec(<<16#83, 1:2, DIG:3, RM:3, D0, D1, I0, I1, 
      Rest/bitstring>>) -> 
    Rest.

xor_dec(Bitstring, Fun) 
  when is_function(Fun) ->
    Fun(),
    xor_dec(Bitstring).
    
    


    
