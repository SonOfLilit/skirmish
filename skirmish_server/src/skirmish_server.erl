%%%-------------------------------------------------------------------
%%% File    : skirmish_server.erl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Skirmish Server application module
%%%
%%% Created : 15 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------
-module(skirmish_server).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%%
%% API functions
%%

start_link() ->
    supervisor:start_link(?MODULE, []).

%%
%% Application callbacks
%%

start(_Type, StartArgs) ->
    case skirmish_server:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.

%%
%% Supervisor callbacks
%%

init([]) ->
    Listener = {listener,{skirmish_server_listener,start_link,[]},
              permanent,2000,worker,[skirmish_server_listener]},
    {ok,{{one_for_all,0,1}, [Listener]}}.

%%
%% Internal functions
%%
