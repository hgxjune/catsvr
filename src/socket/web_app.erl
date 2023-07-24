%%%-------------------------------------------------------------------
%% @doc web_app public API
%% web 端用于游戏服管理、外部 sdk 接入
%% @end
%%%-------------------------------------------------------------------

-module(web_app).
-include("debug.hrl").


-export([start/0, stop/0]).

start() ->
    {ok, App} = application:get_application(),
    case application:get_env(App, web) of
        undefined -> ok;
        {ok, Config} ->
            TransOpts = get_trans_opts(Config),
            ProtoOpts = get_proto_opts(Config),

            {ok, _} = application:ensure_all_started(cowlib),
            {ok, _} = application:ensure_all_started(ranch),
            {ok, _} = application:ensure_all_started(cowboy),

            {ok, _} = cowboy:start_clear(?MODULE, TransOpts, ProtoOpts),
            ok
    end.

stop() ->
    case maps:is_key(?MODULE, ranch:info()) of
        false -> ignore;
        true -> ok = cowboy:stop_listener(?MODULE)
    end,
    ok.

%% internal functions
-spec get_trans_opts(list()) -> ranch:opts().
get_trans_opts(Config) ->
    Port = proplists:get_value(port, Config),
    #{ max_connections  => proplists:get_value(max_connections, Config, 10)
     , num_acceptors    => proplists:get_value(num_acceptors, Config, 2)
     , num_conns_sups   => proplists:get_value(num_conns_sups, Config, 2)
     , socket_opts      => [{port, Port}]
     }.

-spec get_proto_opts(list()) -> cowboy_http:opts().
get_proto_opts(_Config) ->
    Router = {'_', [ {"/[...]", web_echo, []} ]},
    Dispatch = cowboy_router:compile([ Router ]),
    #{ env => #{ dispatch => Dispatch }
     }.


% SocketOpts
% -type opt() :: {backlog, non_neg_integer()}
%     | {buffer, non_neg_integer()}
%     | {delay_send, boolean()}
%     | {dontroute, boolean()}
%     | {exit_on_close, boolean()}
%     | {fd, non_neg_integer()}
%     | {high_msgq_watermark, non_neg_integer()}
%     | {high_watermark, non_neg_integer()}
%     | inet
%     | inet6
%     | {ip, inet:ip_address() | inet:local_address()}
%     | {ipv6_v6only, boolean()}
%     | {keepalive, boolean()}
%     | {linger, {boolean(), non_neg_integer()}}
%     | {low_msgq_watermark, non_neg_integer()}
%     | {low_watermark, non_neg_integer()}
%     | {nodelay, boolean()}
%     | {port, inet:port_number()}
%     | {priority, integer()}
%     | {raw, non_neg_integer(), non_neg_integer(), binary()}
%     | {recbuf, non_neg_integer()}
%     | {send_timeout, timeout()}
%     | {send_timeout_close, boolean()}
%     | {sndbuf, non_neg_integer()}
%     | {tos, integer()}.

% -type transport_opts(SocketOpts) :: #{
%     alarms => #{term() => alarm_num_connections()},
%     connection_type => worker | supervisor,
%     handshake_timeout => timeout(),
%     logger => module(),
%     max_connections => max_conns(),
%     num_acceptors => pos_integer(),
%     num_conns_sups => pos_integer(),
%     num_listen_sockets => pos_integer(),
%     post_listen_callback => fun((term()) -> ok | {error, term()}),
%     shutdown => timeout() | brutal_kill,
%     socket_opts => SocketOpts
% }.
