%%%-------------------------------------------------------------------
%% @doc constant.hrl
%% @end
%%%-------------------------------------------------------------------
-ifndef(CONSTANT_HRL).
-define(CONSTANT_HRL,true).


%% 服务状态
-define(SERVER_STATUS_OPEN,         0).     %% 正常开服
-define(SERVER_STATUS_CLOSE,        1).     %% 已关服
-define(SERVER_STATUS_SUSPEND,      2).     %% 挂起、维护
-define(SERVER_STATUS_READY,        3).     %% 准备阶段



%% 登陆状态
-define(LOGIN_SUCC,                 0).     %% 登陆成功
-define(LOGIN_ERR,                  1).     %% 未知错误





-endif. %% CONSTANT_HRL