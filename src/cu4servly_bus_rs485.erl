-module(cu4servly_bus_rs485).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/0]).
-record(rs485bus_state, {gpio, queue, id}).


%% Interface implementation


start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server callbacks


init(_Args) ->
	G = cu4servly_gpio:init(),
	cu4servly_rs485_sender:init(),
	{ok, #rs485bus_state{gpio = G, id = 0}}.


handle_call({write, Data}, _From, S = #rs485bus_state{gpio = G, id = Id}) ->
	cu4servly_rs485_sender:spawn(Id, G, Data, self()),
	{ok, {queued, Id}, S#rs485bus_state{id = Id + 1}};

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

