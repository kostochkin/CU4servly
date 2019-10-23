-module(cu4servly_gpio).
-export([init/0, stop/1, up/1, down/1]).
-record(gpio_config, {path = "/sys/class/gpio" :: [char()], num = 24 :: integer()}).
-record(gpio, {latch :: tuple(), config :: #gpio_config{}}).
-define(UP, <<"1">>).
-define(DOWN, <<"0">>).
-define(WAIT_LATCH, 1).

%% Interface implementation

-spec init() -> #gpio{}.

init() ->
        init(#gpio_config{}).


-spec stop(#gpio{}) -> ok.

stop(#gpio{latch = L, config = C}) ->
	stop(L, C).


-spec up(#gpio{}) -> ok.

up(G) ->
	up(G, wait).


-spec up(#gpio{}, wait | no_wait) -> ok.

up(#gpio{latch = L}, wait) ->
	latch(L, ?UP),
	wait_latch(L, ?UP);

up(#gpio{latch = L}, no_wait) ->
	latch(L, ?UP).


-spec down(#gpio{}) -> ok.

down(#gpio{latch = L}) ->
	latch(L, ?DOWN).


%% Internal implementation

latch(L, V) ->
	F = file:write(L, V),
	% debug io:format("[ GPIO ] Set latch ~s: ~p~n", [L, F]),
	ok = F.

latch_state(L) ->
	{ok, 0} = file:position(L, 0),
	{ok, R} = file:read(L,1),
	R.

wait_latch(L, State) ->
	timer:sleep(?WAIT_LATCH),
	case latch_state(L) of
		State -> ok;
		_ -> wait_latch(L, State)
	end.


init(GPIO = #gpio_config{}) -> 
	{ok, F} = init(GPIO, 10),
	% debug io:format("[ GPIO ] Port opened.~n"),
	#gpio{latch = F, config = GPIO}.


init(_GPIO, 0) -> {error, cannot_init_gpio};

init(GPIO, Tries) ->
	% debug io:format("[ GPIO ] Init ~p~n", [Tries]),
	case filelib:is_dir(gpio_config(port_path, GPIO)) of
		true ->
			% debug io:format("[ GPIO ] Port exists.~n"),
			ok = file:write_file(gpio_config(direction_path, GPIO), "out"),
			file:open(gpio_config(value_path, GPIO), [read, write, raw, binary]);
		false ->
			% debug io:format("[ GPIO ] Port doesn't exist. Try to create.~n"),
			ok = file:write_file(gpio_config(export_path, GPIO), gpio_config(num, GPIO)),
			timer:sleep(200),
			init(GPIO, Tries - 1)
		end.


stop(F, GPIO) ->
	ok = file:close(F),
	ok = file:write_file(gpio_config(unexport_path, GPIO), gpio_config(num, GPIO)).


gpio_config(num, #gpio_config{num = Num}) when is_list(Num) -> Num;

gpio_config(Name, G = #gpio_config{path = Path, num = Num}) when is_list(Num) ->
	S = case Name of
		port_path -> ["gpio" ++ Num];
		export_path -> ["export"];
		unexport_path -> ["unexport"];
		value_path -> [gpio_config(port_path, G), "value"];
		direction_path -> [gpio_config(port_path, G), "direction"];
		dir_path -> []
	end,
	filename:join([Path | S]);

gpio_config(Name, G = #gpio_config{num = Num}) ->
	gpio_config(Name, G#gpio_config{num = lists:flatten(io_lib:format("~p", [Num]))}).



