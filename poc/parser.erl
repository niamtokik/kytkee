%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(parser).
-compile([export_all]).
-record(segment, { value   :: bitstring() 
		 , size    :: integer()
		 , address :: integer() 
		 , hash    :: [{atom(), bitstring()}, ...]
		 }).

read_file(Filename) 
  when is_list(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Data.

% fun_switch(Fun, Args) 
%  when is_fun(Fun), is_tuple(Args) ->
%    Arity = proplists:get_value(arity, fun_info(Fun))
%    case Arity of
%	1 -> Fun(Args
%	2 -> Fun(proplists:get_value(

parse(Data, {Size, Function}) 
  when is_bitstring(Data), 
       is_integer(Size), 
       is_function(Function) ->
    parse(Data, {Size, Function}, 0);

parse(Data, {Size, Function, Shift})
  when is_bitstring(Data), 
       is_integer(Size), 
       is_function(Function), 
       is_integer(Shift) ->
    <<_:Shift, Shifted/bitstring>> = Data,
    parse(Shifted, {Size, Function}, 0).
    
parse(Data, {Size, Function}, Counter) ->
    case Data of
	<<Head:Size/bitstring, Rest/bitstring>> ->
	    Function({Head, Counter}),
	    parse(Rest, {Size, Function}, 
		  Counter+(trunc(Size/8))); % counter is in octet, not in bit.
	<<Rest/bitstring>> -> 
	    Function({Rest, Counter}),
	    Counter
    end. 

#-spec segment_to_map(#segment{}) -> map().
-spec(segment_to_map/1 :: (#segment{}) -> map()).
segment_to_map(Segment) ->
    #{ value   => get_value(Segment)
     , size    => get_size(Segment) 
     , address => get_address(Segment)
     , hash    => get_hash(Segment) }.

-spec map_to_segment(map()) -> #segment{}.
map_to_segment(Map) ->
    #segment{ value   = maps:get(value, Map)
	    , size    = maps:get(size, Map)
	    , address = maps:get(address, Map)
	    , hash    = maps:get(hash, Map)	      
	    }.
   
-spec get_value(#segment{}) -> bitstring().
get_value(Segment) 
  when is_record(Segment, segment) ->
    #segment{value = Value } = Segment,
    Value.

-spec get_size(#segment{}) -> integer().
get_size(Segment) 
  when is_record(Segment, segment) ->
    #segment{size = Size } = Segment,
    Size.

-spec get_address(#segment{}) -> integer().
get_address(Segment) 
  when is_record(Segment, segment) ->
    #segment{address = Address } = Segment,
    Address.

-spec get_hash(#segment{}) -> [{atom, bitstring()}].
get_hash(Segment) 
  when is_record(Segment, segment) ->
    #segment{hash = Hash} = Segment,
    Hash.

print(Data) ->
    parse(Data, {32, print_fun()}).

print_fun() ->
    fun(S) -> io:format("~p~n", [S]) end.
	    
stat_fun() ->
    fun F(D) -> 
	     
