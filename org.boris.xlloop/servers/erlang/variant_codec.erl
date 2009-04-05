-module(variant_codec).
-export([decode_file/1, decode/1, encode/1]).

-define(STRUCT, 2).
-define(COLLECTION, 3).
-define(STRING, 4).
-define(DOUBLE, 5).
-define(LONG, 6).
-define(NULL, 7).

decode_file(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin).
	
decode(Bin) when is_binary(Bin) ->
	decode(binary_to_list(Bin));
decode(Bin) when is_list(Bin) ->
	[Value|Rest] = Bin,
	case Value of
		?STRUCT -> decode_struct(Rest);
		?COLLECTION -> decode_collection(Rest);
		?STRING -> decode_string(Rest);
		?DOUBLE -> decode_double(Rest);
		?LONG -> decode_long(Rest);
		?NULL -> decode_null(Rest)
	end.
	
decode_struct(Bin) ->
	{Size, Rest} = decode_int(Bin),
	{Value, RestS} = decode_struct(Rest, Size),
	{{struct, Value}, RestS}.

decode_struct(Bin, 0) ->
	{[], Bin};
decode_struct(Bin, Size) ->
	[_|Rest] = Bin,
	{Key, RestK} = decode_string(Rest),
	{Value, RestV} = decode(RestK),
	{Acc, RestAcc} = decode_struct(RestV, Size - 1),
	{[{Key, Value} | Acc], RestAcc}.	

decode_collection(Bin) ->
	{Size, Rest} = decode_int(Bin),
	{Value, RestC} = decode_collection(Rest, Size),
	{{collection, Value}, RestC}.

decode_collection(Bin, 0) ->
	{[], Bin};
decode_collection(Bin, Size) ->
	{Value, Rest} = decode(Bin),
	{V1, R1} = decode_collection(Rest, Size - 1),
	{[Value | V1], R1}.

decode_string(Bin) ->
	{Size, Rest} = decode_int(Bin),
	{Value, RestL} = lists:split(Size, Rest),
	{{string, Value}, RestL}.
	
decode_double(Bin) ->
	{U, Rest} = lists:split(8, Bin),
	<<Value:64/float>> = list_to_binary(U),
	{{double, Value}, Rest}.
	
decode_long(Bin) ->
	{U, Rest} = lists:split(8, Bin),
	<<Value:64>> = list_to_binary(U),
	{{long, Value}, Rest}.

decode_int(Bin) ->
	{U, Rest} = lists:split(4, Bin),
	<<Value:32>> = list_to_binary(U),
	{Value, Rest}.

decode_null(Bin) ->
	{undefined, Bin}.

encode(Value) -> list_to_binary(encode(Value, [])).
	
encode(Value, Acc) when is_tuple(Value) ->
	{Type, Val} = Value,
	case Type of
		struct -> encode_struct(Val, Acc);
		collection -> encode_collection(Val, Acc);
		string -> encode_string(Val, Acc);
		long -> encode_long(Val, Acc);
		double -> encode_double(Val, Acc)
	end;
encode(Value, Acc) when is_atom(Value) ->
	encode_null(Value, Acc).

encode_struct(Value, Acc) ->
	WithType = Acc ++ [?STRUCT],
	WithLen = encode_int(length(Value), WithType),
	Fun = fun(Elem, AccIn) -> 
		{Key, Entry} = Elem,
		encode(Entry, encode(Key, AccIn)) 
	end,		
	lists:foldl(Fun, WithLen, Value).
	
encode_collection(Value, Acc) ->
	WithType = Acc ++ [?COLLECTION],
	WithLen = encode_int(length(Value), WithType),
	Fun = fun(Elem, AccIn) -> 
		encode(Elem, AccIn) 
	end,		
	lists:foldl(Fun, WithLen, Value).

encode_string(Value, Acc) ->
	WithType = Acc ++ [?STRING],
	WithLen = encode_int(length(Value), WithType),
	WithLen ++ Value.

encode_double(Value, Acc) ->
	WithType = Acc ++ [?DOUBLE],
	WithType ++ binary_to_list(<<Value:64/float>>).
	
encode_long(Value, Acc) ->
	WithType = Acc ++ [?LONG],
	WithType ++ binary_to_list(<<Value:64>>).

encode_null(_Value, Acc) ->
	Acc ++ [?NULL].
	
encode_int(Value, Acc) -> 
	Acc ++ binary_to_list(<<Value:32>>).
	
		