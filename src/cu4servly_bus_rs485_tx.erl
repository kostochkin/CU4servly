-module(cu4servly_bus_rs485_tx).
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
	WithCRC = stm32_helper_lib:append_crc32(Data),
	BS = cstarprotocol_bytestuffing_lib:encode(WithCRC),
	gen_server:call(?MODULE, {send, BS}).


%% gen_server callbacks


init(_Args) ->
	{Q, FirstId} = cu4servly_rs485_sender:init_queue(),
	{ok, #rs485bus_state{queue = Q, id = FirstId}}.


handle_call({send, Data}, {From, _Tag}, S = #rs485bus_state{queue = Q, id = Id}) ->
	{NewId, Pid} = cu4servly_rs485_sender:enqueue(Id, Q, Data, From),
	{reply, {enqueued, Pid}, S#rs485bus_state{id = NewId}};

handle_call(Req, From, State) ->
	io:format("[ Bus rs485 tx ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


handle_cast(Req, State) ->
	io:format("[ Bus rs485 tx ] Unknown cast ~p~n", [Req]),
	{noreply, State}.


handle_info(Info, State) ->
	io:format("[ Bus rs485 tx ] Unknown info ~p~n", [Info]),
	{noreply, State}.


terminate(Reason, _State) ->
	io:format("[ Bus rs485 tx ] Terminate with reason ~p~n", [Reason]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	io:format("[ Bus rs485 tx ] Code change not implemented~n", []),
	{ok, State}.


%% Internal implementation

