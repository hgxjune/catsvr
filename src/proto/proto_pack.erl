%% -*- coding: utf-8 -*-
%% @private

-module(proto_pack).

-include("debug.hrl").


-export([pack/3]).
-export([unpack/1]).
-export([route/1]).
-export([route/3]).
-export([test/0]).


%% API
%% -----------------------------------------------------------------------------

%% 
-spec pack(list(), list(), map()) -> binary().
pack(Proto, Name, Data) ->
    Module  = pt2(Proto),
    Bin     = Module:encode_msg(Data, list_to_atom(Name), []),
    Erpc    = #{ module => Proto
               , name => Name
               , bin => Bin
               },
    main_pb2:encode_msg(Erpc, 'ERPC', []).


%%
-spec unpack(binary()) -> {atom(), list(), map()}.
unpack(Bin) ->
    Erpc    = main_pb2:decode_msg(Bin, 'ERPC', []),

    Proto   = maps:get(module, Erpc, undefined),
    Name    = maps:get(name, Erpc, undefined),
    BinData = maps:get(bin, Erpc, <<>>),

    Module  = pt2(Proto),
    Data    = Module:decode_msg(BinData, list_to_atom(Name), []),
    {Proto, Name, Data}.


%% -----------------------------------------------------------------------------

%%
-spec route(binary()) -> ok.
route(Bin) ->
    {Proto, Name, Data} = ?MODULE:unpack(Bin),
    ?MODULE:route(Proto, Name, Data).

-spec route(list(), list(), map()) -> ok.
route(Proto, Name, Data) ->
    Module = rpc(Proto),
    try
        Module:msg(Name, Data)
    catch
        Class:Reason:Stack ->
            ?ERROR("proto route error, Proto:~p, Name:~p, Data:~p~n ~p", [Proto, Name, Data, {Class, Reason, Stack}])
    end,
    ok.


%% -----------------------------------------------------------------------------
pt2("login")    -> login_pb2;
pt2("main")     -> main_pb2;
pt2("role")     -> role_pb2;
pt2("msg")      -> msg_pb2;
pt2(Proto)      -> list_to_atom(Proto ++ "_pb2").


rpc("login")    -> undefined;       %% login 不允许 rpc 路由
rpc("main")     -> main_rpc;
rpc("role")     -> role_rpc;
rpc("msg")      -> msg_rpc;
rpc(Proto)      -> list_to_atom(Proto ++ "_rpc").


%% -----------------------------------------------------------------------------
test() ->
    ok.


