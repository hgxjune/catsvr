%%%-------------------------------------------------------------------
%% @doc id dispenser public API
%% @end
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%% id 分配器，分为 global 、 local 、 template 类型，分配上限 99999999
%% 1. global: 可跨服使用，64 位 id，高 32 = ServerID，低 32 = 本服分配
%% 2. local:  不可跨服使用，32 位 id，为本服分配
%% 3. template: 不可跨服使用，关服后清零，32 位 id
%%%-------------------------------------------------------------------

-module(id_dispenser).
-behaviour(gen_server).

-include("debug.hrl").
-include("ets.hrl").

-record(state,{ server_id
              , role
              , cat
              }
       ).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).
-export([stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([role/0]).
-export([cat/0]).

%% ====================================================================
%% API
%% ====================================================================
role() -> gen_server:call(?MODULE, role).
cat()  -> gen_server:call(?MODULE, cat).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% ====================================================================
init([]) ->
    #{ server_id := ServerID
     , role      := Role
     , cat       := Cat
     } = db_server:load_id(),

    {ok, #state{ server_id = ServerID
               , role = Role
               , cat = Cat
               }
    }.


handle_call(Info, From, State) ->
    try do_call(Info, From, State)
    catch
        Class:Reason:Stack ->
            ?ERROR("handle_call error:~n~p~nstate:~n~p:~nInfo:~p", [{Class, Reason, Stack}, State, Info]),
            {reply, fail, State}
    end.


handle_cast(_Info, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate("server stop", _Status) ->
    ok;

terminate(_Reason, _Status) ->
    ok.


code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.


%% ====================================================================
%% Local functions
%% ====================================================================
do_call(stop, _From, State) ->
    do_save_all(State),
    {stop, "server stop", ok, State};

do_call(role, _From, State) ->
    Role = State#state.role + 1,
    {reply, Role, State#state{role = Role}};

do_call(cat, _From, State) ->
    Cat = State#state.cat + 1,
    {reply, Cat, State#state{cat = Cat}};

do_call(_Info, _From, State) ->
    ?trace(_Info),
    {reply, ok, State}.


do_save_all(State) -> 
    ServerId = State#state.server_id,
    Role     = State#state.role,
    Cat      = State#state.cat,

    db_server:save_id({ServerId, Role, Cat}),
    ok.