-module(variant_codec).
-export([decode_file/1, decode/1]).

decode_file(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin).
	
decode(Bin) when is_binary(Bin) ->
	decode(binary_to_list(Bin));
decode(Bin) when is_list(Bin) ->
	[Value|Rest] = Bin,
	case Value of
		2 -> decode_struct(Rest);
		3 -> decode_collection(Rest);
		4 -> decode_string(Rest);
		5 -> decode_double(Rest);
		6 -> decode_long(Rest);
		7 -> decode_null(Rest)
	end.
	
decode_struct(Bin) ->
	{Size, Rest} = decode_int(Bin),
	decode_struct(Rest, Size).

decode_struct(Bin, 0) ->
	{dict:new(), Bin};
decode_struct(Bin, Size) ->
	[_|Rest] = Bin,
	{Key, Rest2} = decode_string(Rest),
	{Value, Rest3} = decode(Rest2),
	{Dict, Rest4} = decode_struct(Rest3, Size - 1),
	{dict:store(Key, Value, Dict), Rest4}.	

decode_collection(Bin) ->
	{Size, Rest} = decode_int(Bin),
	decode_collection(Rest, Size).

decode_collection(Bin, 0) ->
	{[], Bin};
decode_collection(Bin, Size) ->
	{Value, Rest} = decode(Bin),
	{V1, R1} = decode_collection(Rest, Size - 1),
	{[Value | V1], R1}.

decode_string(Bin) ->
	{Size, Rest} = decode_int(Bin),
	lists:split(Size, Rest).
	
decode_double(Bin) ->
	{U, Rest} = lists:split(8, Bin),
	<<Value:64/float>> = list_to_binary(U),
	{Value, Rest}.
	
decode_long(Bin) ->
	{U, Rest} = lists:split(8, Bin),
	<<Value:64>> = list_to_binary(U),
	{Value, Rest}.

decode_int(Bin) ->
	{U, Rest} = lists:split(4, Bin),
	<<Value:32>> = list_to_binary(U),
	{Value, Rest}.

decode_null(Bin) ->
	{undefined, Bin}.
	