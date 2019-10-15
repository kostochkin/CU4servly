-module(cu4servly_rs485_sender).
%% NOTE: sender is not API, just for suppressing warnings.
-export([init/0, spawn/4, sender/3]).
-define(TIMEOUT, 50).

%% Interface implementation


-spec init() -> ok.

init() ->
	ets:new(?MODULE, [bag, public]),
	ok.

spawn(Id, G, Data, _ReturnPID) ->
	push_queue(Id, Data),
	erlang:spawn(?MODULE, sender, [Id, G, self()]).

pop_queue(Id) ->
	case ets:first(?MODULE) of
		'$end_of_table' -> empty;
		Id -> 
			R = ets:lookup(?MODULE, Id),
			ets:delete(?MODULE, Id),
			R;
		_ -> pop_queue(Id)
	end.


push_queue(Id, Data) ->
	ets:insert(?MODULE, {Id, Data}).


sender(Id, G, ReturnPid) ->
	io:format("[ Bus rs485 ] Sender spawned ~p~n", [self()]),
	case pop_queue(Id) of
		empty ->
			io:format("[ Bus rs485 ] Sender terminated due to empty queue ~p~n", [self()]);
		{Id, Data} ->
			{data, Received} = send_data(G, Data),
			ReturnPid ! {received, Received}
	end.


send_data(G, Data) ->
	S = cu4servly_rs485:init(),
	cu4servly_gpio:up(G),
       	cu4servly_rs485:write(S, Data),
       	timer:sleep(4),
       	cu4servly_gpio:down(G),
       	cu4servly_rs485:read(S).

