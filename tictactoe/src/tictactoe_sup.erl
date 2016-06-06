-module(tictactoe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CLI,      tictactoe_cli)
-define(BOARD,    tictactoe_board)
-define(COMPUTER, tictactoe_computer)

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts     = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy,
              MaxRestarts,
              MaxSecondsBetweenRestarts},

  Restart  = permanent,
  Shutdown = 2000,


  CLIMFA = {?CLI,
            start_link,
            []},

  CLI = {?CLI,
         CLIMFA,
         Restart,
         Shutdown,
         supervisor,
         [?CLI]}, 

  BoardMFA = {?BOARD,
              start_link,
              []},

  Board = {?BOARD,
           BoardMFA,
           Restart,
           Shutdown,
           worker,
           [?BOARD]},


  ChildSpec = [CLI,
               Board],

  {ok, {SupFlags, ChildSpec}}.
