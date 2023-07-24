%%%-------------------------------------------------------------------
%% @doc catsvr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(catsvr_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child_worker/2]).
-export([start_child_supervisor/2]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.


%% -----------------------------------------------------------------------------
start_child_worker(Mod, Args) ->
    ChildSpec = pack_normal_worker(Mod, Args),
    supervisor:start_child(?SERVER, ChildSpec).


start_child_supervisor(Mod, Args) ->
    ChildSpec = pack_normal_srpervisor(Mod, Args),
    supervisor:start_child(?SERVER, ChildSpec).


%% internal functions
pack_normal_worker(Mod, Args) ->
    #{ id       => Mod
     , start    => {Mod, start_link, Args}
     , restart  => permanent
     , shutdown => 5000
     , type     => worker
     , modules  => [Mod]
     }.

pack_normal_srpervisor(Mod, Args) ->
    #{ id       => Mod
     , start    => {Mod, start_link, Args}
     , restart  => transient
     , shutdown => infinity
     , type     => supervisor
     , modules  => [Mod]
     }.
