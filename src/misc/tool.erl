%%%-------------------------------------------------------------------
%% @doc tool public API
%% @end
%%%-------------------------------------------------------------------

-module(tool).

-include("ets.hrl").
-include("debug.hrl").

-export([overview/0]).

-export([server_info/0]).
-export([server_info/1]).

-export([server_id/0]).
-export([server_state/0]).
-export([server_register/0]).
-export([server_login/0]).
-export([server_pay/0]).
-export([server_open_time/0]).

-export([set_server_state/1]).
-export([set_server_register/1]).
-export([set_server_login/1]).
-export([set_server_pay/1]).


%% -----------------------------------------------------------------------------
%% 查看系统状态
overview() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:trunc( erlang:memory(processes_used) ) ,
    ProcMemAlloc = erlang:trunc( erlang:memory(processes) ) ,
    MemTot       = erlang:trunc( erlang:memory(total) ) ,
    io:format( "abormal termination:
              ~n  Scheduler id:                            ~p
              ~n  Num scheduler:                           ~p
              ~n  Process count:                           ~p
              ~n  Process limit:                           ~p
              ~n  Memory used by erlang processes(b):      ~p
              ~n  Memory allocated by erlang processes(b): ~p
              ~n  The total amount of memory allocated(b): ~p
              ~n",
                   [SchedId, SchedNum, ProcCount, ProcLimit,
                    ProcMemUsed, ProcMemAlloc, MemTot]),
    ok.


%% -----------------------------------------------------------------------------
%% 获取服务器信息
server_info() ->
    case ets:lookup(?ETS_CONFIG, server_info) of
        [#ets_config{config = ServerInfo}] ->
            ServerInfo;
        [] ->
            All = catsvr_app:get_config(),
            Keys = [main, id, name, state, register, login, pay, open_time],
            ServerInfo = [proplists:lookup(K, All) || K <- Keys],
            ets:insert(?ETS_CONFIG, #ets_config{name = server_info, config = ServerInfo}),
            ServerInfo
    end.

server_info(Key) ->
    case ets:lookup(?ETS_CONFIG, Key) of
        [#ets_config{config = Value}] ->
            Value;
        [] ->
            All = catsvr_app:get_config(),
            Value = element(2, proplists:lookup(Key, All)),
            ets:insert(?ETS_CONFIG, #ets_config{name = Key, config = Value}),
            Value
    end.

server_id() ->          server_info(id).            % 服务 id
server_state() ->       server_info(state).         % 显示状态
server_register() ->    server_info(register).      % 可否注册新角色
server_login() ->       server_info(login).         % 可否登陆
server_pay() ->         server_info(pay).           % 可否拉起新订单，已支付订单不受影响
server_open_time() ->   server_info(open_time).     % 开服时间

%% -----------------------------------------------------------------------------
%% 设置服务器信息
set_server_info(Key, ST) ->
    ets:insert(?ETS_CONFIG, #ets_config{name = Key, config = ST}).

set_server_state(ST) ->    set_server_info(state, ST).     % 显示状态
set_server_register(ST) -> set_server_info(register, ST).  % 可否注册新角色
set_server_login(ST) ->    set_server_info(login, ST).     % 可否登陆
set_server_pay(ST) ->      set_server_info(pay, ST).       % 可否拉起新订单，已支付订单不受影响


%% internal functions
