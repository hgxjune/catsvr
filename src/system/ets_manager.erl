%%%-------------------------------------------------------------------
%% @doc ets_manager public API
%% @end
%%%-------------------------------------------------------------------

-module(ets_manager).
-behaviour(gen_server).

-include("debug.hrl").
-include("ets.hrl").

-record(state,{}).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
init([]) ->
    init_ets(),
    {ok, #state{}}.


handle_call(_Info, _From, State) ->
    {reply, ok, State}.


handle_cast(_Info, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _Status) ->
    ok.


code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.


%% ====================================================================
%% Local functions
%% ====================================================================
init_ets() ->
    ets:new(?ETS_CONFIG, [{keypos, #ets_config.name}, named_table, public, set]),
    ok.


