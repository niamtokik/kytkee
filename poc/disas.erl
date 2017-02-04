-module(disas).
-compile([export_all]).

% on openbsd you can find all opcode in:
% /usr/src/gnu/usr.bin/binutils-2.17/opcodes

% what is an opcode?
% INSTR ARGS

-define CB 8
-define CW 16
-define CD 32
-define CP 40
-define CO 64
-define CT 80

-define IB 8
-define IW 16
-define ID 32
-define IO 64

-spec disas_file(Filename :: iolist() | string() | atom() ) -> iolist().
disas_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

-spec opcode(Opcode :: iolist() | string() | integer() ) -> tuple().
opcode(Binary) 
  when is_binary(Binary) ->
    Binary.

-spec binary_parser(iolist()) -> ok.
binary_parser(<<>>) ->
    ok;
binary_parser(<<Data, Rest/bitstring>>) ->
    case Data of
	16#0 -> % io:format("NOP~n"),
	     binary_parser(Rest);
	16#1 -> {Eb, Gb, Next} = addB(Rest),
	     binary_parser(Next);
	16#2 -> {Eb, Gv, Next} = addS(Rest),
	     binary_parser(Next);
	16#06 -> {Es, Next} = push(Rest), binary_parser(Next);
	16#0e -> {Es, Next} = push(Rest), binary_parser(Next);
	16#16 -> {Es, Next} = push(Rest), binary_parser(Next);
	16#ff -> {Es, Next} = push(Rest), binary_parser(Next);
	_ -> binary_parser(Rest)
    end.

-spec binary_parser(EntryPoint :: integer(), Data :: iolist()) -> ok.
binary_parser(EntryPoint, Data) 
  when is_integer(EntryPoint), is_binary(Data) ->
    <<_:EntryPoint, Rest/bitstring>> = Data,
    binary_parser(Rest).

switcher(16#06, Data, State) ->
    push;
switcher(16#0E, Data, State) ->
    push;
switcher(16#16, Data, State) ->
    push;
switcher(16#ff, Data, State) ->
    push;
switcher(16#50, Date, State) ->
    push.


aaa(<<16#37, Rest/bitstring>>) ->
    ok.

aad(<<16#D5, 16#0A, Rest/bitstring>>) ->
    ok;
aad(<<16#D5, Ib:?IB, Rest/bitstring>>) ->
    ok.

aam(<<16#D4, 16#0A, Rest/bitstring>>) ->
    ok;
aam(<<16#D4, Ib:?IB, Rest/bitstring>>) ->
    ok.

aas(<<16#3F, Rest/bitstring>>) ->
    ok.

adc(<<16#14, Ib:?IB, Rest/bitstring>>) ->
    ok;
adc(<<16#15, Iw:?IW, Rest/bitstring>>) ->
    ok;
adc(<<16#15, Id:?ID, Rest/bitstring>>) ->
    ok.


add(<<16#04, Ib:?IB, Rest/bitstring>>) ->
    ok;
add(<<16#05, Iw:?IW, Rest/bitstring>>) ->
    ok;
add(<<16#05, Id:?ID, Rest/bitstring>>) ->
    ok.








addB (<<Eb:64, Gb:64, Rest/bitstring>>) ->
    io:format("AddB -> ~.16b ~.16b ~n", [Eb, Gb]),
    {Eb, Gb, Rest}.

addS(<<Eb:64, Gv:64, Rest/bitstring>>) ->
    io:format("AddS -> ~.16b ~.16b ~n", [Eb, Gv]),
    {Eb, Gv, Rest}.

push(<<Es:64, Rest/bitstring>>) ->
    io:format("Push -> ~.16b ~n", [Es]),
    {Es, Rest}.
     
pop(<<Es:64, Rest/bitstring>>) ->
    io:format("Pop -> ~.16b ~n", [Es]),
    {Es, Rest}.
