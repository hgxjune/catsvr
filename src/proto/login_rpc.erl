%% -*- coding: utf-8 -*-
%% @private

-module(login_rpc).

-include("debug.hrl").
-include("record.hrl").
-include("constant.hrl").


-export([msg/3]).


%% API
%% -----------------------------------------------------------------------------

%% 
-spec msg(atom(), map(), atom()) -> term().
msg("CS_server_info", _Data, State) ->
    SCServerInfo = #{ server => tool:server_id()
                    , status => tool:server_state()
                    , open_date => tool:server_open_time()
                    },
    Bin = proto_pack:pack("login", "SC_server_info", SCServerInfo),
    {Bin, State};


msg("CS_user", #{uid := UID}, State) ->
    %% 数据库获取列表
    UserList = [#{server => ServerId, rid => RID} || {ServerId, RID} <- db_user:get_user(UID)],
    SCUser = #{user => UserList},
    Bin = proto_pack:pack("login", "SC_user", SCUser),
    {Bin, State};


msg("CS_login", #{uid := UID, server := ServerId, info := Info, sign := Sign}, State) ->
    Code = do_login(UID, ServerId, Info, Sign),
    SCLogin = #{code => Code},
    Bin = proto_pack:pack("login", "SC_login", SCLogin),
    {Bin, State};


msg(_Name, _Data, _State) ->
    ?WARNING("not have name: ~p, data: ~p", [_Name, _Data, _State]),
    ok.


%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
do_login(UID, ServerId, _Info, Sign) ->
    try
        ok = chech_login(Sign),
        RoleList = db_user:get_user(UID),
        RID = lists:keyfind(ServerId, 1, RoleList),
        do_login_role(UID, ServerId, RID),
        ?LOGIN_SUCC
    catch
        throw:MsgId ->
            MsgId;
        Class:Reason:Stack ->
            ?ERROR("user login error, UID: ~p, Class:~p, Reason:~p Stack:~n~p", [UID, Class, Reason, Stack]),
            ?LOGIN_ERR
    end.


chech_login(_Sign) ->
    ok.


do_login_role(UID, ServerId, false) ->
    RID = do_register_role(ServerId, UID),
    do_login_role(ServerId, UID, {ServerId, RID});
do_login_role(UID, ServerId, {ServerId, RID}) ->
    ok.


do_register_role(UID, ServerId) ->
    RID    = id_dispenser:role(),
    Base   = #r_role_base{},
    Res    = #r_role_res{},
    Cat    = #r_cat{},
    Cats   = [Cat],
    Stage  = #r_stage{},
    Stages = [Stage],

    db_user:reg_user(UID, ServerId, RID),
    db_role:reg_role(RID, UID, ServerId, Base, Res, Cats, Stages),
    RID.
