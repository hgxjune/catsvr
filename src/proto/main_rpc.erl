%% -*- coding: utf-8 -*-
%% @private

-module(main_rpc).

-include("debug.hrl").


-export([msg/3]).


%% API
%% -----------------------------------------------------------------------------

%% 
-spec msg(atom(), map(), atom()) -> term().
msg("Heart", Data, _State) ->
    ?trace("-- tiktok: ~p", [Data]),
    {ok, proto_pack:pack("main", "Heart", Data)};


msg(_Name, _Data, _State) ->
    ?WARNING("not have name: ~p, data: ~p", [_Name, _Data, _State]),
    ok.


%% -----------------------------------------------------------------------------


