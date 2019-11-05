-module(cu4servly_bus_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => cu4servly_bus_rs485_sup, start => {cu4servly_bus_rs485_sup, start_link, []}},
		  #{id => cu4servly_bus_unit_sup, start => {cu4servly_bus_unit_sup, start_link, []}},
		  #{id => cu4servly_bus, start => {cu4servly_bus, start, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
