%%%-------------------------------------------------------------------
%% @doc db_server public API
%% @end
%%%-------------------------------------------------------------------

-module(db_server).

-include("debug.hrl").
-include("constant.hrl").

-export([server_info/0]).
-export([load_id/0, save_id/1]).


%%%-------------------------------------------------------------------
%%% server 表
% #{ server_id  => ServerId
%  , status => Status
%  , open_time => OpenTime
%  }
% ServerId  :: integer;
% Status    :: integer;
% OpenTime  :: binstring;

%-spec server_info(ServerId) -> #{}.
server_info() ->
    Connection = db:connect(),

    ServerId = tool:server_id(),
    Collection = <<"server">>,
    Selector = #{ <<"server_id">> => db:to_binstring( ServerId ) },

    Result =
    case db:find_one(Connection, Collection, Selector) of
        undefined ->
            Server = #{ <<"server_id">> => db:to_binstring(ServerId)
                      , <<"status">>    => db:to_binstring(tool:server_state())
                      , <<"open_time">> => db:to_binstring(tool:server_open_time())
                      },
            db:insert(Connection, Collection, Server),
            Server;
        Data ->
            Data
    end,

    db:disconnect(Connection),

    #{ server_id => db:binstring_to_integer( maps:get(server_id, Result) )
     , status    => db:binstring_to_integer( maps:get(status,    Result) )
     , open_time => db:binstring_to_string(  maps:get(open_time, Result) )
     }.


%%%-------------------------------------------------------------------
%%% id dispenser
% #{ server_id => ServerId
%  , role => RoleId
%  , cat => CatId
%  }
% ServerId :: integer;
% RoleId   :: integer;
% CatId    :: integer;

%-spec load_id() -> #{}.
load_id() ->
    Connection = db:connect(),

    ServerId   = tool:server_id(),
    Collection = <<"id_dispenser">>,
    Selector   = #{ <<"server_id">> => ServerId },

    Result =
    case db:find_one(Connection, Collection, Selector) of
        undefined ->
            <<BaseId:64>> = <<ServerId:32, 100:32>>,
            ID = #{ <<"server_id">> => ServerId
                  , <<"role">>      => BaseId
                  , <<"cat">>       => BaseId
                  },
            db:insert(Connection, Collection, ID),
            ID;
        ID ->
            ID
    end,

    db:disconnect(Connection),
    {ServerId, maps:get(<<"role">>, Result), maps:get(<<"cat">>, Result)}.


%-spec save_id() -> ok.
save_id({ServerId, Role, Cat}) ->
    Connection = db:connect(),

    Collection = <<"id_dispenser">>,
    Selector   = #{ <<"server_id">> => ServerId },
    ID         = #{ <<"server_id">> => ServerId
                  , <<"role">>      => Role
                  , <<"cat">>       => Cat
                  },

    db:update(Connection, Collection, Selector, ID),
    
    db:disconnect(Connection),
    ok.
