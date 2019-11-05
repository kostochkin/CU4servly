-module(cu4servly_bus_unit).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/2]).
-include("unit.hrl").
-include("unit_general_commands.hrl").

-record(init_state, {unit :: #unit{}, tries :: integer(), replier :: pid()}).


%% Interface implementation


% -spec start() -> {ok, pid()}.

start(#unit{} = Unit, Tries) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, #init_state{unit = Unit, tries = Tries}, []).


%% gen_server callbacks


init(InitState) ->
	{ok, init_state(InitState)}.


handle_call(Req, From, State) ->
	io:format("[ Bus unit ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


handle_cast({received, Sender, <<A, ?G_GetDeviceType, _Rest/binary>>},
	    #init_state{replier = Sender, unit = #unit{address = A} = U}) ->
	io:format("[ Unit ] Unit enumerated ~p~n", [A]),
	{noreply, U};

handle_cast({received, Sender, _}, S = #init_state{replier = Sender}) ->
	NewState = #init_state{tries = NT} = init_state(S),
	if
		NT < 1 ->
			io:format("[ Unit ] Unit not found ~p~n", [S#init_state.unit]),
			{stop, unit_not_found, NewState};
		true ->
			{noreply, NewState}
	end;

handle_cast(Req, S) ->
	io:format("[ Unit ] Unknown cast ~p~n", [Req]),
	{noreply, S}.


handle_info(Info, State) ->
	io:format("[ Bus unit ] Unknown info ~p~n", [Info]),
	{noreply, State}.


terminate(Reason, _State) ->
	io:format("[ Bus unit ] Terminate with reason ~p~n", [Reason]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	io:format("[ Bus unit ] Code change not implemented~n", []),
	{ok, State}.


%% Internal implementation

init_state(S = #init_state{unit = #unit{address = A}, tries = T}) ->
	{enqueued, Replier} = cu4servly_bus_rs485_tx:send(<<A, ?G_GetDeviceType, 0>>),
	S#init_state{tries = T - 1, replier = Replier}.

