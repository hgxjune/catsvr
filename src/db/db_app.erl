%%%-------------------------------------------------------------------
%% @doc db_app public API
%% @end
%%%-------------------------------------------------------------------

-module(db_app).
-include("ets.hrl").
-define(DB, mongodb).

-export([start/0, stop/0]).
-export([get_config/0]).


start() ->
    {ok, _} = application:ensure_all_started(?DB),

    {ok, App} = application:get_application(),
    {ok, Config} = application:get_env(App, ?DB),
    set_config(Config),
    ok.

stop() ->
    ok.


%% -----------------------------------------------------------------------------
set_config(Config) ->
    ets:insert(?ETS_CONFIG, #ets_config{name = ?DB, config = Config}).

get_config() ->
    case ets:lookup(?ETS_CONFIG, ?DB) of
        [#ets_config{config = Config}] -> Config;
        _ -> []
    end.


%% internal functions

% database
% -type arg() :: {database, database()}
% | {login, binary()}
% | {password, binary()}
% | {w_mode, write_mode()}
% | {r_mode, read_mode()}
% | {host, list()}
% | {port, integer()}
% | {register, atom() | fun()}
% | {next_req_fun, fun()}.

