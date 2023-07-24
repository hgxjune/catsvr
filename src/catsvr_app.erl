%%%-------------------------------------------------------------------
%% @doc catsvr public API
%% @end
%%%-------------------------------------------------------------------

-module(catsvr_app).

-behaviour(application).

-include("ets.hrl").

-export([start/2, stop/1]).
-export([get_config/0]).

-define(APPS, [db_app, chat, metronome, tcp_app, web_app, websocket_app]).

start(_StartType, _StartArgs) ->
    {ok, SupPid} = catsvr_sup:start_link(),
    {ok, _} = catsvr_sup:start_child_worker(ets_manager, []),
    set_config(),

    [App:start() || App <- ?APPS],

    {ok, _} = catsvr_sup:start_child_worker(id_dispenser, []),
    

    {ok, SupPid}.


stop(_State) ->
    id_dispenser:stop(),
    
    [App:stop() || App <- lists:reverse(?APPS)],
    ok.


%% -----------------------------------------------------------------------------
set_config() ->
    {ok, App} = application:get_application(),
    Config = application:get_all_env(App),
    ets:insert(?ETS_CONFIG, #ets_config{name = ?MODULE, config = Config}).


get_config() ->
    case ets:lookup(?ETS_CONFIG, ?MODULE) of
        [#ets_config{config = Config}] -> Config;
        _ -> []
    end.
