%%%-------------------------------------------------------------------
%% @doc cu4servly public API
%% @end
%%%-------------------------------------------------------------------

-module(cu4servly_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cu4servly_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
