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
-define(SREF, 9).

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
		?INT -> decode_int(Rest);
		?SREF -> decode_sref(Rest)
	end.
	
decode_num(Bin) ->
    {U, Rest} = lists:split(8, Bin),
    <<Value:64/float>> = list_to_binary(U),
    {Value, Rest}.
	
decode_str(Bin) ->
    [Size|Rest] = Bin,
    lists:split(Size, Rest).

decode_bool(Bin) ->
    [B|Rest] = Bin,
	if 
		B == 0 -> {false, Rest};
		true -> {true, Rest}
	end.
	
decode_err(Bin) ->
	{Err, Rest} = decode_int(Bin),
	{{err, Err}, Rest}.

decode_missing(Bin) ->
	{undefined, Bin}.

decode_int(Bin) ->
	{U, Rest} = lists:split(4, Bin),
	<<Value:32>> = list_to_binary(U),
	{Value, Rest}.
	
decode_sref(Bin) ->
	{ColFirst, Rest1} = decode_int(Bin),
	{ColLast, Rest2} = decode_int(Rest1),
	{RowFirst, Rest3} = decode_int(Rest2),
	{RowLast, Rest4} = decode_int(Rest3),
	{{sref, [ColFirst,ColLast,RowFirst,RowLast]}, Rest4}.

decode_multi(Bin, Rows, _Cols, Acc) when Rows == 0 -> {Acc, Bin};
decode_multi(Bin, Rows, Cols, Acc) ->
	{Value, Rest} = decode_multi(Bin, Cols, []),
	decode_multi(Rest, Rows-1, Cols, [Acc|Value]).
decode_multi(Bin, Length, Acc) when Length == 0 -> {Acc, Bin};
decode_multi(Bin, Length, Acc) ->
	{Value, Rest} = decode(Bin),
	decode_multi(Rest, Length-1, Acc ++ [Value]).
decode_multi(Bin) ->
	{Rows, R1} = decode_int(Bin),
	{Cols, R2} = decode_int(R1),
	Length = Rows * Cols,
	if
		Rows == 0 ; Cols == 0 ->
			{[], R2};
		Rows == 1; Cols == 1 ->
			decode_multi(R2, Length, []);
		true ->
			decode_multi(R2, Rows, Cols, [])
	end.

encode(Value) -> list_to_binary(encode(Value, [])).
	
encode(Value, Acc) when is_tuple(Value) ->
	{Type, Val} = Value,
	case Type of
		num -> encode_num(Val, Acc);
		str -> encode_str(Val, Acc);
		err -> encode_err(Val, Acc);
		_ -> encode_null(Value, Acc)
	end;
encode(Value, Acc) when is_atom(Value) ->
	if
		Value == true -> Acc ++ [?BOOL, 1];
		Value == false -> Acc ++ [?BOOL, 0];
		true -> encode_null(Value, Acc)
	end;
encode(Value, Acc) when is_number(Value) ->
	encode_num(Value, Acc).

encode_str(Value, Acc) ->
	Acc ++ [?STR, length(Value)] ++ Value.

encode_num(Value, Acc) ->
	Acc ++ [?NUM] ++ binary_to_list(<<Value:64/float>>).

encode_err(Value, Acc) ->
	Acc ++ [?ERR] ++ binary_to_list(<<Value:32>>).
	
encode_null(_Value, Acc) ->
	Acc ++ [?NIL].
	
	
		