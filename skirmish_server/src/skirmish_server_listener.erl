%%%-------------------------------------------------------------------
%%% File    : skirmish_server_listener.erl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Listens for new connections and spawns a process to
%%%               handle them
%%%
%%% Created : 15 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------
-module(skirmish_server_listener).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_link/1,
	 set_dimensions/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/skirmish_server.hrl").

-define(SERVER, ?MODULE). % Internal registered name of the server process
-define(DEFAULT_PORT, 1657). % Default port to listen on TODO: DRY with client

-record(state, {socket, dimensions}).

%%
%% API
%%
start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
    

set_dimensions(X, Y, W, H) ->
    gen_server:call(?SERVER, {set_dimensions, [X, Y, W, H]}).

%%
%% gen_server callbacks
%%

init([]) ->
    init([?DEFAULT_PORT]);
init([Port]) ->
    init([Port, [0, 0, 3000, 3000]]);
init([Port, Dimensions]) ->
    {ok, Socket} = gen_udp:open(Port, [list, {active,true}]),
    {ok, #state{socket=Socket, dimensions=Dimensions}}.

handle_call({set_dimensions, Dim}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{dimensions=Dim}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({udp, Socket, IP, InPortNo, Packet},
	    State=#state{socket=Socket, dimensions=Dimensions}) ->
    skirmish_server_client_handler:start({IP, InPortNo}, Dimensions, Packet),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}) ->
    gen_udp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%
