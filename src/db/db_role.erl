%%%-------------------------------------------------------------------
%% @doc db_role public API
%% @end
%%%-------------------------------------------------------------------

-module(db_role).

-include("record.hrl").
-include("debug.hrl").

-export([reg_role/7]).
-export([get_role/1]).

%%% role 表
% #{ rid  => RID
%  , uid  => UID
%  , server_id => ServerId
%  , base => #r_role_base{}
%  , res => #r_role_res{}
%  , cat => []
%  , stage => []
%  }
% UID       :: binstring;
% RID       :: integer;
% ServerId  :: integer;


reg_role(RID, UID, ServerId, Base, Res, Cat, Stage) ->
    Role = #{ rid => RID
            , uid => db:to_binstring(UID)
            , server_id => ServerId
            , base => db:term_to_binstring(Base)
            , res => db:term_to_binstring(Res)
            , cat => db:term_to_binstring(Cat)
            , stage => db:term_to_binstring(Stage)
            },

    Connection = db:connect(),
    Collection = <<"role">>,
    db:insert(Connection, Collection, Role),
    db:disconnect(Connection),
    ok.


%-spec get_user(UID) -> [{ServerId, RID}].
get_role(RID) ->
    Connection = db:connect(),

    Collection = <<"role">>,
    Selector = #{ <<"rid">> => RID },

    Role = db:find_one(Connection, Collection, Selector),

    db:disconnect(Connection),

    %RID, UID, ServerId, Base, Res, Cat, Stage.
    { RID
    , db:binstring_to_string( maps:get(<<"uid">>, Role) )
    , maps:get(<<"server_id">>, Role)
    , db:binstring_to_term( maps:get(<<"base">>, Role) )
    , db:binstring_to_term( maps:get(<<"res">>, Role) )
    , db:binstring_to_term( maps:get(<<"cat">>, Role) )
    , db:binstring_to_term( maps:get(<<"stage">>, Role) )
    }.






%% -----------------------------------------------------------------------------
