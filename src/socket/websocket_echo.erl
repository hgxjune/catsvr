%%%-------------------------------------------------------------------
%% @doc websocket_echo public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket_echo).
-include("debug.hrl").
-include("record.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


init(Req, Opts) ->
    State = #r_websocket{ req = Req
                        , opts = Opts
                        , status = empty
                        , role_id = 0
                        , role_pid = undefined
    },
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    ?trace("listening connect: ~p", [State]),
    {[], State}.


websocket_handle({binary, Body}, State) when State#r_websocket.status == online ->
    gen_server:cast(State#r_websocket.role_pid, {'SOCKET_EVENT', Body}),
    {[], State};

websocket_handle({binary, Body}, State) ->
    try
        {Bin, State1} = do_unpack(Body, State),
        {[{binary, Bin}], State1}
    catch
        Class:Reason:Stack ->
            ?trace("state(~p) ~nlogin critical error, Class:~p, Reason:~p, Stack:~p", [State, Class, Reason, Stack]),
            {stop, State}
    end;
    
websocket_handle(_Data, State) ->
    ?trace(_Data),
    {[], State}.


websocket_info({send, Bin}, State) ->
    {[{binary, Bin}], State};
websocket_info(_Info, State) ->
    ?trace(_Info),
    {[], State}.


%% internal functions
%% 未登录，只处理 login 和 main 消息
%% 登陆完成后，将不再处理 login 消息
do_unpack(Body, State) ->
    case proto_pack:unpack(Body) of
        {"login", Name, Data} ->
            do_login(Name, Data, State);
        {"main", Name, Data} ->
            do_main(Name, Data, State);
        _ -> 
            {<<>>, State}
    end.

do_login(Name, Data, State) ->
    login_rpc:msg(Name, Data, State).

do_main(Name, Data, State) ->
    {ok, Bin} = main_rpc:msg(Name, Data, State),
    {Bin, State}.
