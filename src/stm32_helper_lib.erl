-module(stm32_helper_lib).
-export([crc32/1, has_correct_crc32/1, append_crc32/1, remove_crc32/1]).

%% Stm32 Crc32.
%%
%% http://we.easyelectronics.ru/STM32/crc32-na-stm32-kak-na-kompe-ili-na-kompe-kak-na-stm32.html
%%
%% >> CRC32 на STM32 можно посчитать так же, как и принято в ethernet.
%% >> Для этого надо реверсировать входные слова и реверсировать в итоге контрольную сумму.
%% >> Это работает только для длины, кратной 4.
%%
%% (bits 0, 1, 2, ... 29, 30, 31) -> (bits 32, 30, 29, ... 2, 1, 0)
%%

-spec remove_crc32( MsgCrc :: binary() ) -> Msg :: binary().
remove_crc32(MsgCrc) ->
	Size = byte_size(MsgCrc),
	binary_part(MsgCrc, {0, Size - 4}).

-spec append_crc32( Msg :: binary() ) -> MsgCrc :: binary().
append_crc32(Msg) ->
	Crc32 = crc32(Msg),
 	<<Msg/binary, Crc32:32/native>>.


-spec has_correct_crc32( MsgCrc :: binary () ) -> boolean().
has_correct_crc32(MsgCrc) ->
	Size = byte_size(MsgCrc),
	Message = remove_crc32(MsgCrc),
	<<Crc:32/native>> = binary_part(MsgCrc, {Size, -4}),
	Crc == crc32(Message).

-spec crc32( binary() ) -> integer().
crc32(Binary) -> crc32(Binary, 0).

-spec crc32( binary(), integer() ) -> integer().
crc32(<<>>, Crc) -> 
 	<<CrcR:32>> = reverse(<<Crc:32>>, <<>>),
	CrcR;
crc32(<<Word:4/binary, Rest/binary>>, OldCrc) ->
	Crc = erlang:crc32(OldCrc, reverse(Word, <<>>)),
	crc32(Rest, Crc);
crc32(Binary, OldCrc) -> crc32(<<Binary/binary, 0>>, OldCrc).
	
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/bits, Rest/bits>>, Acc) ->
	reverse(Rest, <<H/bits, Acc/bits>>).

