-module(cu4servly_rs485_sender).
%% NOTE: sender is not API, just for suppressing warnings.
-export([init_queue/0, enqueue/3, sender/3]).
-define(TIMEOUT, 50).

%% Interface implementation


-spec init_queue() -> ok.

init_queue() ->
	ets:new(?MODULE, [bag, public]).

enqueue(Ets, Data, _ReturnPID) ->
	spawn(?MODULE, sender, [Ets, Data, self()]).

wait_queue(Ets, Id) ->
	case ets:first(Ets) of
		'$end_of_table' -> empty;
		Id -> Id;
		_ -> wait_queue(Ets, Id)
	end.


push_queue(Ets, Id) ->
	ets:insert(Ets, {Id, ok}).


pop_queue(Ets, Id) ->
	ets:delete(Ets, Id).


sender(Ets, Data, ReturnPid) ->
	io:format("[ Bus rs485 ] Sender spawned ~p~n", [self()]),
	Id = self(),
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
       	timer:sleep(4),
       	cu4servly_gpio:down(G),
       	cu4servly_rs485:read(S).

