-module(xloper_codec).
-export([decode_file/1, decode/1, encode/1]).

-define(NUM, 1).
-define(STR, 2).
-define(BOOL, 3).
-define(ERR, 4).
-define(MULTI, 5).
-define(MISSING, 6).
-define(NIL, 7).
-define(INT, 8).

decode_file(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin).
	
decode(Bin) when is_binary(Bin) ->
	decode(binary_to_list(Bin));
decode(Bin) when is_list(Bin) ->
	[Value|Rest] = Bin,
	case Value of
        ?NUM -> decode_num(Rest);
        ?STR -> decode_str(Rest);
        ?BOOL -> decode_bool(Rest);
        ?ERR -> decode_err(Rest);
		?MULTI -> decode_multi(Rest);
		?MISSING -> decode_missing(Rest);
		?NIL -> decode_missing(Rest);
		?INT -> decode_int(Rest)
	end.
	
decode_num(Bin) ->
    {U, Rest} = lists:split(8, Bin),
    <<Value:64/float>> = list_to_binary(U),
    {Value, Rest}.
	
decode_str(Bin) ->
    {Size, Rest} = decode_int(Bin),
    lists:split(Size, Rest).

decode_bool(Bin) ->
    {Size, Rest} = decode_int(Bin),
    lists:split(Size, Rest).
	
decode_err(Bin) ->
	{Err, Rest} = decode_int(Bin),
	{{err, Err}, Rest}.

decode_missing(Bin) ->
	{undefined, Bin}.

decode_int(Bin) ->
	{U, Rest} = lists:split(4, Bin),
	<<Value:32>> = list_to_binary(U),
	{Value, Rest}.

decode_multi(Bin, Rows, Cols, Acc) when Rows == 0 -> Acc,
decode_multi(Bin, Rows, Cols, Acc) ->
	decode_multi(Bin, Rows-1, Cols, [Acc|decode_multi(Bin, Cols, [])]).
decode_multi(Bin, Length, Acc) when Length == 0 -> Acc,
decode_multi(Bin, Length, Acc) ->
	decode_multi(Bin, Length-1, [Acc|decode_xloper(Bin)]).
decode_multi(Bin) ->
	{Rows, R1} = decode_int(Bin),
	{Cols, R2} = decode_int(R1),
	Length = Rows * Cols,
	if
		Rows == 0 ; Cols == 0 ->
			[];
		Rows == 1; Cols == 1 ->
			decode_multi(Bin, Length, []);
		_ ->
			decode_multi(Bin, Rows, Cols, [])
	end.

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
	
		