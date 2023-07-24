%%%-------------------------------------------------------------------
%% @doc get_user public API
%% @end
%%%-------------------------------------------------------------------

-module(db_user).

-include("debug.hrl").

-export([get_user/1]).
-export([reg_user/3]).

%%% user 表
% #{ uid  => UID
%  , role => [{ServerId, RID}, {ServerId, RID}]
%  }
% UID       :: binstring;
% ServerId  :: integer;
% RID       :: integer;


%-spec get_user(UID) -> [{ServerId, RID}].
get_user(UID) ->
    Connection = db:connect(),

    Collection = <<"user">>,
    Selector = #{ <<"uid">> => db:to_binstring(UID) },

    Result =
    case db:find_one(Connection, Collection, Selector) of
        undefined ->
            User = #{ <<"uid">>  => db:to_binstring(UID)
                    , <<"role">> => []
                    },
            db:insert(Connection, Collection, User),
            User;
        Data ->
            Data
    end,

    db:disconnect(Connection),
    [ {ServerId, RID} || #{ <<"server_id">> := ServerId, <<"rid">> := RID } <- maps:get(<<"role">>, Result) ].


%-spec reg_user(UID, ServerId, RID) -> ok.
reg_user(UID, ServerId, RID) ->
    Connection = db:connect(),

    Collection = <<"user">>,
    Selector = #{ <<"uid">> => db:to_binstring(UID) },
    Role = #{ <<"server_id">> => ServerId, <<"rid">> => RID },

    User = db:find_one(Connection, Collection, Selector),
    RoleList = db:keystore(<<"server_id">>, ServerId, maps:get(<<"role">>, User), Role),
    db:update(Connection, Collection, Selector, User#{<<"role">> := RoleList}),

    db:disconnect(Connection),
    ok.



%% -----------------------------------------------------------------------------
