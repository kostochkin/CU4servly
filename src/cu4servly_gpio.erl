-module(cu4servly_gpio).
-export([init/0]).
-record(serial_port, {device = "/dev/ttyS0", speed = 500000}).
-record(gpio_port, {path = "/sys/class/gpio", num = 24}).
%-record(gpio_state, {port, latch}).


init() -> init1(#serial_port{}, #gpio_port{}, 10).


init1(_SP, _GPIO, 0) -> {error, cannot_init_gpio};
init1(SP, GPIO, Tries) ->
	io:format("[ GPIO ] Init ~p~n", [Tries]),
	case filelib:is_dir(gpio_port(port_path, GPIO)) of
		true ->
			io:format("[ GPIO ] Port exists~n"),
			ok = file:write_file(gpio_port(direction_path, GPIO), "out"),
			file:open(gpio_port(value_path, GPIO), [read, write, raw, exclusive, binary]);
		false ->
			io:format("[ GPIO ] Port doesn't exist. Try to create.~n"),
			ok = file:write_file(gpio_port(export_path, GPIO), gpio_port(num, GPIO)),
			timer:sleep(200),
			init1(SP, GPIO, Tries - 1)
		end.


gpio_port(num, #gpio_port{num = Num}) when is_list(Num) -> Num;
gpio_port(Name, G = #gpio_port{path = Path, num = Num}) when is_list(Num) ->
	S = case Name of
		port_path -> ["gpio" ++ Num];
		export_path -> ["export"];
		value_path -> [gpio_port(port_path, G), "value"];
		direction_path -> [gpio_port(port_path, G), "direction"];
		dir_path -> []
	end,
	filename:join([Path | S]);


gpio_port(Name, G = #gpio_port{num = Num}) ->
	gpio_port(Name, G#gpio_port{num = lists:flatten(io_lib:format("~p", [Num]))}).



