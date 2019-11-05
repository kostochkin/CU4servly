-module(cu4servly_bus_unit).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/2]).
-include("unit.hrl").
-include("unit_general_commands.hrl").

-record(state, {name = enumerate :: enumerate | get_modification | ready,
	       unit :: #unit{}, tries :: {integer(), integer()}, replier :: pid() | undefined}).


%% Interface implementation


% -spec start() -> {ok, pid()}.

start(#unit{} = Unit, Tries) ->
	{ok, _} = R = gen_server:start_link(
			?MODULE,
			#state{unit = Unit, tries = {Tries, Tries}}, []),
	R.


%% gen_server callbacks


init(InitState) ->
	{ok, init_state(InitState)}.


handle_call(Req, From, State) ->
	io:format("[ Bus unit ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


handle_cast({received, Sender, <<A, ?G_GetDeviceType, Length, Type:Length/binary>>},
	    I = #state{name = enumerate, replier = Sender, unit = #unit{address = A} = U}) ->
	<<"CU4", DT:2/binary, _Rest/binary>> = Type,
	UnitType = proplists:get_value(DT, ?UNIT_TYPES),
	Unit = U#unit{type = #unit_type{t = UnitType}},
	io:format("[ Unit ] Unit enumerated ~p ~p~n", [A, Unit]),
	case UnitType of
		undefined ->
			{stop, {shutdown, {unknown_type, UnitType}}, Unit};
		_ ->
			NS = init_state(I#state{name = get_modification, unit = Unit}),
			{noreply, reset_tries(NS)}
	end;

handle_cast({received, Sender, <<A, ?G_GetModVersion, Length, Version:Length/binary>>},
	    I = #state{name = enumerate, replier = Sender, unit = #unit{address = A} = U}) ->
	Unit = U#unit{type = U#unit_type{m = Version}},
	io:format("[ Unit ] Unit ~p version ~p~n", [A, Unit]),
	{noreply, I#state{name = ready, unit = Unit}};

handle_cast({received, Sender, _}, S = #state{replier = Sender, tries = {T, _}})
	when T > 0 ->
	{noreply, init_state(S)};

handle_cast({received, Sender, _}, S = #state{replier = Sender, tries = {0, _}}) ->
	{stop, {shutdown, unit_not_responding}, S};

handle_cast(Req, S) ->
	io:format("[ Unit ] Unknown cast ~p~n", [Req]),
	{noreply, S}.


handle_info(Info, State) ->
	io:format("[ Bus unit ] Unknown info ~p~n", [Info]),
	{noreply, State}.


terminate(Reason, State) ->
	io:format("[ Bus unit ] Terminate with reason ~p, state ~p~n", [Reason, State]),
	ok.


code_change(_OldVsn, State, _Extra) ->
	io:format("[ Bus unit ] Code change not implemented~n", []),
	{ok, State}.


%% Internal implementation

init_state(S = #state{name = enumerate, unit = #unit{address = A}, tries = {T, FT}}) ->
	{enqueued, Replier} = cu4servly_bus_rs485_tx:send(<<A, ?G_GetDeviceType, 0>>),
	S#state{tries = {T - 1, FT}, replier = Replier};

init_state(S = #state{name = get_modification, unit = #unit{address = A}, tries = {T, FT}}) ->
	{enqueued, Replier} = cu4servly_bus_rs485_tx:send(<<A, ?G_GetModVersion, 0>>),
	S#state{tries = {T - 1, FT}, replier = Replier}.

reset_tries(S = #state{tries = {_, T}}) ->
	S#state{tries = {T, T}}.
