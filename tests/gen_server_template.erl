%%%-------------------------------------------------------------------
%% @doc gen_server_template public API
%% @end
%%%-------------------------------------------------------------------

-module(gen_server_template).
-behaviour(gen_server).

-include("debug.hrl").

-define(TIMEOUT, 300).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {v1, v2}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
init([]) ->
    {ok, #state{v1=[], v2=0}}.


handle_call(Info, From, State) ->
    try do_call(Info, From, State)
    catch
        Class:Reason:Stack ->
            ?ERROR("handle_call error:~n~p~nstate:~n~p:~nInfo:~p", [{Class, Reason, Stack}, State, Info]),
            {reply, fail, State}
    end.


handle_cast(Info, State) ->
    try do_cast(Info, State)
    catch
        Class:Reason:Stack ->
            ?ERROR("handle_cast error:~n~p~nstate:~n~p:~nInfo:~p", [{Class, Reason, Stack}, State, Info]),
            {noreply, State}
    end.


handle_info(Info, State) ->
    try do_info(Info, State)
    catch
        Class:Reason:Stack ->
            ?ERROR("handle_info error:~n~p~nstate:~n~p:~nInfo:~p", [{Class, Reason, Stack}, State, Info]),
            {noreply, State}
    end.


terminate(_Reason, _Status) ->
    ok.


code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.


%% ====================================================================
%% Local functions
%% ====================================================================
do_call(_Info, _From, State) ->
    ?trace(_Info),
    {reply, ok, State}.


%% --------------------------------------------------------------------
do_cast(_Info, State) ->
    ?trace(_Info),
    {noreply, State}.


%% --------------------------------------------------------------------
do_info(_Info, State) ->
    ?trace(_Info),
    {noreply, State}.


%% --------------------------------------------------------------------
