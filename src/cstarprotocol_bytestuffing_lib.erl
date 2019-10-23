-module(cstarprotocol_bytestuffing_lib).
%-ifdef(TEST).
%-include("cstarprotocol_bytestuffing_test.hrl").
%-else.
-include("cstarprotocol_bytestuffing.hrl").
%-endif.
-include("cstarprotocol_bytestuffing_decoder_state.hrl").
-export([encode/1, decode/1, decode/2]).

-spec encode( binary() ) -> binary().
encode(Message) ->
	<<?StartByte, (escape_bytes(Message))/binary, ?StopByte>>.
	

-spec decode( binary(), #decoder_state{} ) -> {ok, #decoder_state{}} | {error, #decoder_state{}}.
decode(Message, State = #decoder_state{rest = Rest}) ->
	decode(State#decoder_state{rest = <<Rest/binary, Message/binary>>}).

-spec decode( binary() | #decoder_state{} ) -> {ok, #decoder_state{}} | {error, #decoder_state{}}.
decode(Message) when is_binary(Message) ->
	State = #decoder_state{},
	decode(State#decoder_state{rest = Message});

decode(S = #decoder_state{rest = <<?StartByte, Rest/binary>>}) ->
	decode(S#decoder_state{state = ?DECODER_STATE_SEEK_STOP, result = <<>>, rest = Rest});
decode(S = #decoder_state{state = ?DECODER_STATE_SEEK_START, rest = <<_, Rest/binary>>}) ->
	decode(S#decoder_state{rest = Rest});
decode(S = #decoder_state{state = ?DECODER_STATE_SEEK_STOP, rest = <<?StopByte, Rest/binary>>}) ->
	{ok, S#decoder_state{state = ?DECODER_STATE_SEEK_START, rest = Rest}};
decode(S = #decoder_state{state = ?DECODER_STATE_SEEK_STOP, rest = <<?EscapeByte, Rest/binary>>}) ->
	decode(S#decoder_state{state = ?DECODER_STATE_ESCAPED, rest = Rest});
decode(S = #decoder_state{state = ?DECODER_STATE_SEEK_STOP, result = Acc, rest = <<X, Rest/binary>>}) ->
	decode(S#decoder_state{result = <<Acc/binary, X>>, rest = Rest});
decode(S = #decoder_state{state = ?DECODER_STATE_ESCAPED}) ->
	decode_escaped(S);
decode(S) ->
	{error, S}.

decode_escaped(S = #decoder_state{result = Acc, rest = <<?EscapeSubstByte, Rest/binary>>}) ->
	decode(S#decoder_state{state = ?DECODER_STATE_SEEK_STOP, result = <<Acc/binary, ?EscapeByte>>, rest = Rest});
decode_escaped(S = #decoder_state{result = Acc, rest = <<?StartSubstByte, Rest/binary>>}) ->
	decode(S#decoder_state{state = ?DECODER_STATE_SEEK_STOP, result = <<Acc/binary, ?StartByte>>, rest = Rest});
decode_escaped(S = #decoder_state{result = Acc, rest = <<?StopSubstByte, Rest/binary>>}) ->
	decode(S#decoder_state{state = ?DECODER_STATE_SEEK_STOP, result = <<Acc/binary, ?StopByte>>, rest = Rest});
decode_escaped(S = #decoder_state{rest = <<>>}) ->
	{error, S};
decode_escaped(#decoder_state{rest = Rest}) ->
	decode(Rest).


escape_bytes(Message) -> escape_bytes(Message, <<>>).

escape_bytes(<<>>, Acc) -> Acc;
escape_bytes(<<?EscapeByte, Rest/binary>>, Acc) ->
	escape_bytes(Rest, <<Acc/binary, ?EscapeByte, ?EscapeSubstByte>>);
escape_bytes(<<?StartByte, Rest/binary>>, Acc) ->
	escape_bytes(Rest, <<Acc/binary, ?EscapeByte, ?StartSubstByte>>);
escape_bytes(<<?StopByte, Rest/binary>>, Acc) ->
	escape_bytes(Rest, <<Acc/binary, ?EscapeByte, ?StopSubstByte>>);
escape_bytes(<<X, Rest/binary>>, Acc) ->
	escape_bytes(Rest, <<Acc/binary, X>>).

