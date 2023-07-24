%%%-------------------------------------------------------------------
%% @doc record.hrl
%% @end
%%%-------------------------------------------------------------------
-ifndef(RECORD_HRL).
-define(RECORD_HRL,true).

%%% net
-record(r_websocket, {req, opts, status, role_id, role_pid}).
-record(r_tcp, {ref, transport, opts, socket, status, role_id, role_pid}).


%%% role
%% 基础属性
-record(r_role_base, {rid, icon, level, exp}).
%% 资源
-record(r_role_res, {rid, coin, silver, gold, rmb}).
%% 猫猫
% [ #r_cat{} ]
%% 关卡
% [ #r_stage{} ]



%%% 关卡
-record(r_stage, {id, map}).

%%% cat
-record(r_cat, {catid, breed, level, exp}).


-endif. %% RECORD_HRL