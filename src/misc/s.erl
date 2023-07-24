%%%-------------------------------------------------------------------
%% @doc s public API
%% 对 c 模块进行简单包装
%% @end
%%%-------------------------------------------------------------------

-module(s).
-include("debug.hrl").


-export([ l/0
        , l/1
        , r/0
        ]).

%% 重新载入默认 app
l() ->
    LoadApps = [App || {App, _Description, _Vsn} <- application:loaded_applications()],
    l(LoadApps).

%% 重新载入，需要指定 app
l(LoadApps) when is_list(LoadApps) ->
    F = fun(App, List) ->
        {ok, MS} = application:get_key(App, modules),
        List ++ MS
    end,
    Modules = lists:foldl(F, [], LoadApps),
    update(Modules),
    ok;
l(LoadApp) -> l([LoadApp]).


%% 重新载入所有已经载入过的模块
r() ->
    Modules = erlang:loaded(),
    update(Modules),
    ok.

%% internal functions
update(Modules) ->
    update_loop(Modules, [], []).

update_loop([Module | Modules], Succ, Fail) ->
    case do_update(Module) of
        ignore ->
            update_loop(Modules, Succ, Fail);
        {error, Info} ->
            update_loop(Modules, Succ, [{Module, Info} | Fail]);
        {module, Module} ->
            update_loop(Modules, [Module | Succ], Fail)
    end;
update_loop([], [], []) ->
    ?trace("nothing updated!!!");
update_loop([], Succ, []) ->
    ?trace("succ: ~p", [Succ]);
update_loop([], [], Fail) ->
    ?trace("fail: ~p", [Fail]);
update_loop([], Succ, Fail) ->
    ?trace("succ: ~p", [Succ]),
    ?trace("fail: ~p", [Fail]).

do_update(Module) ->
    case code:module_status(Module) of
        modified ->
            soft_update(Module);
        not_loaded ->
            ignore;
        loaded ->
            ignore;
        removed ->
            {error, "file removed"}
    end.

soft_update(Module) ->
    case code:soft_purge(Module) of
        true ->
            code:load_file(Module);
        false ->
            {error, "not purge"}
    end.

