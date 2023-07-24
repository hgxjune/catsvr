%%%-------------------------------------------------------------------
%% @doc role_send public API
%% @end
%%%-------------------------------------------------------------------

-module(role_send).

-include("debug.hrl").


-export([create_sender/3]).
-export([loop/3]).
-export([send/1]).
-export([send/2]).

%% 
create_sender(Socket, Transport, RoleId) ->
    Sender = erlang:spawn_link(fun() -> ?MODULE:loop(Socket, Transport, RoleId) end),
    OldSender = erlang:put(role_sender, Sender),
    stop_old_sender(OldSender).

stop_old_sender(undefined) -> ok;
stop_old_sender(OldSender) -> OldSender ! stop.


%% 
loop(Socket, Transport, RoleId) ->
    receive
        {send, Bin} ->
            try
                Transport:send(Socket, Bin)
            catch
                Class:Reason:Stack ->
                    ?trace("send loop error, role id: ~p, error:~n~p",[RoleId, {Class, Reason, Stack}])
            end,
            ?MODULE:loop(Socket, Transport, RoleId);
        stop ->
            ok;
        _ ->
            ?MODULE:loop(Socket, Transport, RoleId)
    end.


%%
send(Msg) ->
    Pid = erlang:get(role_sender),
    send(Pid, Msg).


send(Pid, Msg) ->
    Pid ! {send, Msg}.
