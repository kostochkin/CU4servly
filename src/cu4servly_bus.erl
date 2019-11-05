-module(cu4servly_bus).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/0]).
-export([enumerate/1]).

-include("unit.hrl").
-record(bus_state, {units}).

-define(RS485_CHANNELS, [0,1,2]).
-define(ENUMERATE_TRIES, 10).


%% Interface implementation



-spec start() -> {ok, pid()}.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enumerate(rs485) ->
	gen_server:call(?MODULE, {enumerate, rs485, ?ENUMERATE_TRIES}).


%% gen_server callbacks


init(_Args) ->
	{ok, #bus_state{units = init_units_db()}}.


handle_call({enumerate, rs485, Tries}, _From, State) ->
	Unit = fun (A) -> #unit{interface = rs485, address = A} end,
	[supervisor:start_child(cu4servly_bus_unit_sup, [Unit(X), self(), Tries]) || X <- ?RS485_CHANNELS],
	{reply, {ok, queued}, State};

handle_call(Req, From, State) ->
	io:format("[ Bus rs485 rx ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


%handle_cast({data, Pid, From, Data}, State) ->
%	gen_server:handle_cast(Pid, {received, From, Data}),
%	{noreply, State};

handle_cast(Req, State) ->
	io:format("[ Bus rs485 rx ] Unknown cast ~p~n", [Req]),
	{noreply, State}.


handle_info(Info, State) ->
	io:format("[ Bus rs485 rx ] Unknown info ~p~n", [Info]),
	{noreply, State}.


terminate(Reason, _State) ->
	io:format("[ Bus rs485 rx ] Terminate with reason ~p~n", [Reason]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	io:format("[ Bus rs485 rx ] Code change not implemented~n", []),
	{ok, State}.


%% Internal implementation

init_units_db() ->
	ets:new(units, [bag]).



