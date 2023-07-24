%% Feel free to use, reuse and abuse the code in this file.

-module(tcp_echo).
-behaviour(ranch_protocol).
-include("debug.hrl").
-include("record.hrl").


-export([start_link/3]).
-export([init/3]).
-export([login/3]).
-export([loop/3]).


start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, Opts) ->
    process_flag(trap_exit, true),
    {ok, Socket} = ranch:handshake(Ref),

    State = #r_tcp{ ref = Ref
                  , transport = Transport
                  , opts = Opts
                  , socket = Socket
                  , status = empty
                  , role_id = 0
                  , role_pid = undefined
                  },
    ?MODULE:login(Socket, Transport, State).

% ------------------------------------------------------------------------------
%% 登陆处理，需要和其他处理分离
login(Socket, Transport, State) when State#r_tcp.status == online ->
    ?MODULE:loop(Socket, Transport, State);

%% 游戏处理，发生错误掉线
login(Socket, Transport, State) ->
    try
        {ok, Body} = Transport:recv(Socket, 0, 60000),
        {Bin, State1} = do_unpack(Body, State),
        Transport:send(Socket, Bin),
        ?MODULE:login(Socket, Transport, State1)
    catch
        Class:Reason:Stack ->
            ?trace("state(~p) ~nlogin critical error, Class:~p, Reason:~p, Stack:~p", [State, Class, Reason, Stack]),
            {stop, State}
    end.


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


% ------------------------------------------------------------------------------
%% 游戏处理，发生错误不掉线
loop(Socket, Transport, State) ->
    try
        {ok, Body} = Transport:recv(Socket, 0, 60000),
        gen_server:cast(State#r_tcp.role_pid, {'SOCKET_EVENT', Body})
    catch
        Class:Reason:Stack ->
            ?trace("state(~p) critical error:~p", [State, {Class, Reason, Stack}])
    end,
    ?MODULE:loop(Socket, Transport, State).




