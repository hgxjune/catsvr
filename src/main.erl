%%%-------------------------------------------------------------------
%% @doc catsvr main public API
%% @end
%%%-------------------------------------------------------------------

-module(main).
-include("debug.hrl").

-define(APP, catsvr).

-export([start/0, stop/0]).
-export([log_test_/0]).

start() ->
    %{ok, _} = application:ensure_all_started(lager),
	{ok, _} = application:ensure_all_started(?APP),

    ok.

stop() ->
    application:stop(?APP),
    ok.


log_test_() ->
    ?CRITICAL("critical msg"),
    ?ERROR("error msg"),
    ?WARNING("warning msg"),
    ?NOTICE("notice msg"),
    ?INFO("info msg"),
    ?trace,
    ?trace("trace msg"),
    ?trace("trace msg:~p", ["test"]),
    erlang:spawn_link(fun() -> erlang:throw("this is test") end),
    ok.
