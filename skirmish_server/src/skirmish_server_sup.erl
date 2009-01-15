%%%-------------------------------------------------------------------
%%% File    : skirmish_server_sup.erl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Skirmish Server top supervisor
%%%
%%% Created : 15 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------
-module(skirmish_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Listener = {listener,{skirmish_server_listener,start_link,[]},
              permanent,2000,worker,[skirmish_server_listener]},
    {ok,{{one_for_all,0,1}, [Listener]}}.

%%====================================================================
%% Internal functions
%%====================================================================
