%%%-------------------------------------------------------------------
%%% @author Reid
%%% @copyright (C) 2016, Reid
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-01 12:35:44.530293
%%%-------------------------------------------------------------------
-module(hash_table_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER, hash_table).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts     = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy,
              MaxRestarts,
              MaxSecondsBetweenRestarts},

  Restart  = permanent,
  Shutdown = 2000,
  Type     = worker,

  WorkerMFA = {?WORKER,
               start_link,
               []},

  Worker = {?WORKER,
            WorkerMFA,
            Restart,
            Shutdown,
            Type,
            [?WORKER]},

  ChildSpec = [Worker],

  {ok, {SupFlags, ChildSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
