-module(cu4servly_bus_rs485_rx).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
-export([start/0]).
-include("cstarprotocol_bytestuffing_decoder_state.hrl").


%% Interface implementation


-spec start() -> {ok, pid()}.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server callbacks


init(_Args) ->
	{ok, []}.


handle_call(Req, From, State) ->
	io:format("[ Bus rs485 rx ] Unknown call ~p from ~p~n", [Req, From]),
	{reply, [], State}.


handle_cast({data, To, From, Data}, State) ->
	Rec = case cstarprotocol_bytestuffing_lib:decode(Data) of
		{ok, #decoder_state{result = R}} ->
			       case stm32_helper_lib:has_correct_crc32(R) of
				       true ->
					       stm32_helper_lib:remove_crc32(R);
				       false ->
					       <<>>
			       end;
		{error, _} ->
			       <<>>
	end,
	gen_server:cast(To, {received, From, Rec}),
	{noreply, State};

handle_cast({error, _To, _From, {Reason, _}},  State) ->
	io:format("[ Bus rs485 rx ] Sender error ~p~n", [Reason]),
	{noreply, State};

handle_cast(Req,  State) ->
	io:format("[ Bus rs485 rx ] Unknown handle cast ~p~n", [Req]),
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

