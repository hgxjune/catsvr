%%%-------------------------------------------------------------------
%% @doc main_test public API
%% @end
%%%-------------------------------------------------------------------

-module(main_test).

-include("./tests/debug_test.hrl").

-export([run/0]).

run() ->
    ?trace("main_test begin --"),
    {ok, _} = application:ensure_all_started(sasl),
    _Pid = spawn_link(fun loop/0),

    ?trace("main_test end --"),
    ?trace(?OS),
    ok.

loop() ->
    receive
        stop ->
            ?trace("test process stop.");
        Msg ->
            ?trace(Msg),
            loop()
    after
        1000 ->
            exit(timeout)
    end.
