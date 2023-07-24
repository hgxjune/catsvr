%%%-------------------------------------------------------------------
%% @doc debug.hrl
%% @end
%%%-------------------------------------------------------------------
-ifndef(DEBUG_HRL).
-define(DEBUG_HRL,true).

% -define(CRITICAL(Msg),          lager:critical(Msg)).
% -define(CRITICAL(Fmt, Args),    lager:critical(Fmt, Args)).
% -define(ERROR(Msg),             lager:error(Msg)).
% -define(ERROR(Fmt, Args),       lager:error(Fmt, Args)).
% -define(WARNING(Msg),           lager:warning(Msg)).
% -define(WARNING(Fmt, Args),     lager:warning(Fmt, Args)).
% -define(NOTICE(Msg),            lager:notice(Msg)).
% -define(NOTICE(Fmt, Args),      lager:notice(Fmt, Args)).
% -define(INFO(Msg),              lager:info(Msg)).
% -define(INFO(Fmt, Args),        lager:info(Fmt, Args)).

% -define(trace(Fmt, Args),       lager:info("~p:[~p] -- " ++ Fmt, [?MODULE, ?LINE | Args])).
% -define(trace(Arg),             lager:info("~p:[~p] -- ~p", [?MODULE, ?LINE, Arg])).
% -define(trace,                  lager:info("~p:[~p] --", [?MODULE, ?LINE])).


%% windows 开发环境下，不使用生产环境
-include_lib("kernel/include/logger.hrl").

-define(CRITICAL(Msg),          ?LOG_CRITICAL(Msg)).
-define(CRITICAL(Msg, Args),    ?LOG_CRITICAL(Msg, Args)).
-define(ERROR(Msg),             ?LOG_ERROR(Msg)).
-define(ERROR(Msg, Args),       ?LOG_ERROR(Msg, Args)).
-define(WARNING(Msg),           ?LOG_WARNING(Msg)).
-define(WARNING(Msg, Args),     ?LOG_WARNING(Msg, Args)).
-define(NOTICE(Msg),            ?LOG_NOTICE(Msg)).
-define(NOTICE(Msg, Args),      ?LOG_NOTICE(Msg, Args)).
-define(INFO(Msg),              ?LOG_INFO(Msg)).
-define(INFO(Fmt, Args),        ?LOG_INFO(Fmt, Args)).


-define(trace(Format, Args),    ?LOG_NOTICE("~p:[~p] -- " ++ Format, [?MODULE, ?LINE | Args])).
-define(trace(Arg),             ?LOG_NOTICE("~p:[~p] -- ~p", [?MODULE, ?LINE, Arg])).
-define(trace,                  ?LOG_NOTICE("~p:[~p] --", [?MODULE, ?LINE])).


-endif. %% DEBUG_HRL