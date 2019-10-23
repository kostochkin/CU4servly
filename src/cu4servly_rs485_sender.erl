-module(cu4servly_rs485_sender).
%% NOTE: sender is not API, just for suppressing warnings.
-export([init_queue/0, enqueue/4, sender/4]).
-define(TIMEOUT, 50).
-define(WAIT_WRITE, 5).

% {ok, P} = cu4servly_bus_rs485:start().
% gen_server:call(P, {send, <<16#f9, 16#00, 16#06, 16#00, 16#22, 16#9d, 16#2d, 16#dd, 16#fa>>}).

%% Interface implementation


-spec init_queue() -> ok.

init_queue() ->
	{ets:new(?MODULE, [ordered_set, public]), 0}.

enqueue(Id, Ets, Data, _ReturnPID) ->
	{Id + 1, spawn(?MODULE, sender, [Id, Ets, Data, self()])}.

wait_queue(Ets, Id) ->
	case ets:first(Ets) of
		'$end_of_table' -> empty;
		Id -> Id;
		_ -> wait_queue(Ets, Id)
	end.


push_queue(Ets, Id) ->
	ets:insert(Ets, {Id, self()}).


pop_queue(Ets, Id) ->
	ets:delete(Ets, Id).


sender(Id, Ets, Data, ReturnPid) ->
	io:format("[ Bus rs485 ] Sender spawned, ID ~p, PID ~p ~n", [Id, self()]),
	push_queue(Ets, Id),
	case wait_queue(Ets, Id) of
		empty ->
			io:format("[ Bus rs485 ] Sender terminated due to empty queue ~p~n", [self()]);
		Id ->
			{data, Received} = send_data(Data),
			pop_queue(Ets, Id),
			ReturnPid ! {received, Received}
	end.


send_data(Data) ->
	G = cu4servly_gpio:init(),
	S = cu4servly_rs485:init(),
	cu4servly_gpio:up(G),
       	cu4servly_rs485:write(S, Data),
       	timer:sleep(?WAIT_WRITE),
       	cu4servly_gpio:down(G),
       	cu4servly_rs485:read(S),
	cu4servly_rs485:stop(S).

