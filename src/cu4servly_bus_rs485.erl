-module(cu4servly_bus_rs485).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/0, send/1]).
-record(rs485bus_state, {queue, id}).


%% Interface implementation


-spec start() -> {ok, pid()}.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec send(binary()) -> {enqueued, pid()}.

send(Data) ->
	gen_server:call(?MODULE, Data).


%% gen_server callbacks


init(_Args) ->
	{Q, FirstId} = cu4servly_rs485_sender:init_queue(),
	{ok, #rs485bus_state{queue = Q, id = FirstId}}.


handle_call({send, Data}, From, S = #rs485bus_state{queue = Q, id = Id}) ->
	{NewId, Pid} = cu4servly_rs485_sender:enqueue(Id, Q, Data, From),
	{reply, {enqueued, Pid}, S#rs485bus_state{id = NewId}};

handle_call(Req, From, State) ->
	io:format("[ Bus rs485 ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


handle_cast(Req, State) ->
	io:format("[ Bus rs485 ] Unknown cast ~p~n", [Req]),
	{noreply, State}.


handle_info(Info, State) ->
	io:format("[ Bus rs485 ] Unknown info ~p~n", [Info]),
	{noreply, State}.


terminate(Reason, _State) ->
	io:format("[ Bus rs485 ] Terminate with reason ~p~n", [Reason]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	io:format("[ Bus rs485 ] Code change not implemented~n", []),
	{ok, State}.


%% Internal implementation

