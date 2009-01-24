%% @author Aur Saraf <sonoflilit@gmail.com>
%%
%% @doc Skirmish Server application module

-module(skirmish_server).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([start_link/1,
	 set_world_dimensions/4]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% == API functions ==

%% @spec start_link(Args) -> Result
%% Args = [term()]
%% Result = {ok,Pid} | ignore | {error,Error}
%% Pid = pid()
%% Error = {already_started,Pid} | shutdown | term()
%%
%% @doc Start the skirmish server
%%
%% `Args' is passed to skirmish_server_listener:start_link/1
%% @see skirmish_server_listener:start_link/1
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%% @see skirmish_server_listener/4
set_world_dimensions(X, Y, W, H) ->
    skirmish_server_listener:set_world_dimensions(X, Y, W, H).


%% == Application callbacks ==

%% @private
start(_Type, StartArgs) ->
    case skirmish_server:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%% @private
stop(_State) ->
    ok.

%% == Supervisor callbacks ==

%% @private
%% @doc Starts a skirmish_server_listener worker.
init(Args) ->
    Listener = {listener,{skirmish_server_listener,start_link,Args},
		permanent,2000,worker,[skirmish_server_listener]},
    {ok,{{one_for_all,0,1}, [Listener]}}.

%% == Internal functions ==
