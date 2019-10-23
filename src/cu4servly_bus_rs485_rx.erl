-module(cu4servly_bus_rs485_rx).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/0]).


%% Interface implementation


-spec start() -> {ok, pid()}.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server callbacks


init(_Args) ->
	{ok, []}.


handle_call(Req, From, State) ->
	io:format("[ Bus rs485 ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


handle_cast({data, Pid, Data}, State) ->
	gen_server:handle_cast(Pid, {received, Data}),
	{noreply, State};

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

