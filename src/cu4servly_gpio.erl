-module(cu4servly_gpio).
-export([init/0]).
-record(gpio_config, {path = "/sys/class/gpio", num = 24}).
-record(gpio, {latch, config}).


init() ->
        init(#gpio_config{}).

init(GPIO = #gpio_config{}) -> 
	{ok, F} = init(GPIO, 10),
	io:format("[ GPIO ] Port opened.~n"),
	#gpio{latch = F, config = GPIO}.


init(_GPIO, 0) -> {error, cannot_init_gpio};

init(GPIO, Tries) ->
	io:format("[ GPIO ] Init ~p~n", [Tries]),
	case filelib:is_dir(gpio_config(port_path, GPIO)) of
		true ->
			io:format("[ GPIO ] Port exists.~n"),
			ok = file:write_file(gpio_config(direction_path, GPIO), "out"),
			file:open(gpio_config(value_path, GPIO), [read, write, raw, binary]);
		false ->
			io:format("[ GPIO ] Port doesn't exist. Try to create.~n"),
			ok = file:write_file(gpio_config(export_path, GPIO), gpio_config(num, GPIO)),
			timer:sleep(200),
			init(GPIO, Tries - 1)
		end.


gpio_config(num, #gpio_config{num = Num}) when is_list(Num) -> Num;

gpio_config(Name, G = #gpio_config{path = Path, num = Num}) when is_list(Num) ->
	S = case Name of
		port_path -> ["gpio" ++ Num];
		export_path -> ["export"];
		value_path -> [gpio_config(port_path, G), "value"];
		direction_path -> [gpio_config(port_path, G), "direction"];
		dir_path -> []
	end,
	filename:join([Path | S]);

gpio_config(Name, G = #gpio_config{num = Num}) ->
	gpio_config(Name, G#gpio_config{num = lists:flatten(io_lib:format("~p", [Num]))}).



