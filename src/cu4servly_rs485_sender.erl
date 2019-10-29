-module(cu4servly_rs485_sender).
%% NOTE: sender is not API, just for suppressing warnings.
-export([init_queue/0, enqueue/4, sender/4]).
-define(TIMEOUT, 50).
-define(WAIT_WRITE, 5).
-define(WAIT_QUEUE, 1).

% {ok, P} = cu4servly_bus_rs485:start().
% gen_server:call(P, {send, <<16#f9, 16#00, 16#06, 16#00, 16#22, 16#9d, 16#2d, 16#dd, 16#fa>>}).

%% Interface implementation


-spec init_queue() -> ok.

init_queue() ->
	{ets:new(?MODULE, [ordered_set, public]), 0}.

enqueue(Id, Ets, Data, ReturnPID) ->
	{Id + 1, spawn(?MODULE, sender, [Id, Ets, Data, ReturnPID])}.

wait_queue(Ets, Id) ->
	timer:sleep(?WAIT_QUEUE),
	case ets:first(Ets) of
		'$end_of_table' -> empty;
		Id -> Id;
		_ -> wait_queue(Ets, Id)
	end.


push_queue(Ets, Id) ->
	ets:insert(Ets, {Id, self()}).


pop_queue(Ets, Id) ->
	ets:delete(Ets, Id).


sender(Id, Queue, Data, ReturnPid) ->
	io:format("[ Bus rs485 ] Sender spawned, ID ~p, PID ~p ~n", [Id, self()]),
	push_queue(Queue, Id),
	try
		Id = wait_queue(Queue, Id),
		case send_data(Data) of
			{ok, R} ->
				gen_server:cast(cu4servly_bus_rs485_rx, {data, ReturnPid, self(), R});
			{error, R} ->
				gen_server:cast(cu4servly_bus_rs485_rx, {error, ReturnPid, self(), R})
		end,
		pop_queue(Queue, Id)
	catch
		X ->
			gen_server:cast(cu4servly_bus_rs485_rx, {error, ReturnPid, self(), undefined}),
			pop_queue(Queue, Id),
			throw(X)
	end.


send_data(Data) ->
	G = cu4servly_gpio:init(),
	S = cu4servly_rs485:init(),
	cu4servly_gpio:up(G),
       	cu4servly_rs485:write(S, Data),
       	timer:sleep(?WAIT_WRITE),
       	cu4servly_gpio:down(G),
       	R = cu4servly_rs485:read(S),
	cu4servly_rs485:stop(S),
	R.

