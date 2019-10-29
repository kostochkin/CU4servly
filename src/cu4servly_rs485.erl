-module(cu4servly_rs485).
-export([init/0, stop/1, write/2, read/1]).
-record(serial_port_config, {device = "/dev/ttyS0" :: [char()], speed = 500000 :: integer()}).
-record(rs485, {stream :: pid(), owner :: pid(), config :: #serial_port_config{}}).
-define(TIMEOUT, 50).

%% Interface implementation


-spec init() -> #rs485{}.

init() -> 
	init(#serial_port_config{}).


-spec stop(#rs485{}) -> ok.

stop(#rs485{stream = Pid}) ->
	Pid ! {close},
	Pid ! stop,
	wait_stop(Pid).


-spec write(#rs485{}, Data :: binary()) -> ok.

write(#rs485{stream = S}, Data) ->
	% debug io:format("[ RS485 ] Sending ~p~n", [Data]),
	S ! {send, Data}.


-spec read(#rs485{}) -> {data, Data :: binary()} | {error, Reason :: atom()}.

read(#rs485{owner = O}) ->
	case self() of
		O -> read1(<<>>);
		_ -> {error, current_pid_is_not_owner}
	end.


%% Internal implementation


init(C = #serial_port_config{device = D, speed = S}) ->
	Pid = serial:start([{open, D}, {speed, S}]),
	Owner = self(),
	% debug io:format("[ RS485 ] ~s opened, baudrate ~p, PID ~p, Owner ~p~n", [D, S, Pid, Owner]),
	#rs485{stream = Pid, owner = Owner, config = C}.


read1(Data) when byte_size(Data) < 5000 ->
	receive
		{data, B} ->
			% debug io:format("[ RS485 ] Readed ~p~n", [B]),
		       	read1(<<Data/binary, B/binary>>);
		_ ->
			{error, unknown_message}
	after
	       ?TIMEOUT -> {ok, Data}
	end;

read1(Data) ->
	{error, {too_big_packet, Data}}.


wait_stop(Pid) ->
	case is_process_alive(Pid) of
		true -> wait_stop(Pid);
		false -> ok
	end.

