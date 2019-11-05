-module(cu4servly_lib).
-export([mk_enum/1, mk_enum/2]).


-spec mk_enum(Enums) -> EnumToInt when
        Enum :: atom(),
	Enums :: list(Enum),
	EnumToInt :: fun((Enum) -> integer()).

mk_enum(List) ->
	mk_enum(List, 0).


-spec mk_enum(Enums, StartNum) -> EnumToInt when
        Enum :: atom(),
	Enums :: list(Enum),
	StartNum :: integer(),
	EnumToInt :: fun((Enum) -> integer()).

mk_enum(L, S) ->
	fun (Needle) -> index_of(Needle, L, S) end.


index_of(Needle, [Needle | _], N) -> N;
index_of(Needle, [_ | Rest], N) -> index_of(Needle, Rest, N + 1);
index_of(Needle, _, _) -> throw({invalid_enum_value, Needle}).

