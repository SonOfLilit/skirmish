%% @author Aur Saraf <sonoflilit@gmail.com>
%%
%% @doc Listens for new connections (by default on port 1657) and
%% spawns a skirmish_server_client_handler to handle each.
%%
%% @see skirmish_server_client_handler

-module(skirmish_server_listener).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_link/1,
	 set_world_dimensions/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/skirmish_server.hrl").

-define(SERVER, ?MODULE). % Internal registered name of the server process
-define(DEFAULT_PORT, 1657). % Default port to listen on TODO: DRY with client

-record(state, {socket, dimensions}).

%% == API ==

%% @equiv start_link([])
start_link() ->
    start_link([]).


%% @spec start_link(Args::list()) -> Result
%% Result = {ok,Pid} | ignore | {error,Error}
%% Pid = pid()
%% Error = {already_started,Pid} | term()
%%
%% @doc Start a new listener, listening on the default port 1657.
%% 
%% The first item in `Args', if present, is the port to listen on.
%%
%% The second item in `Args', if present, is a list `[X, Y, W, H]' of
%% the world dimensions.
%%
%% @see set_world_dimensions/4
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% @spec set_world_dimensions(X::integer(), Y::integer(),
%% W::integer(), H::integer()) -> ok
%%
%% @doc Set location of next game in coordinate-space (`X', `Y' of
%% top-left corner of the world; `W'idth and `H'eight of the world).
set_world_dimensions(X, Y, W, H) ->
    gen_server:call(?SERVER, {set_world_dimensions, [X, Y, W, H]}).

%% == gen_server callbacks ==

%% @private
init([]) ->
    init([?DEFAULT_PORT]);
init([Port]) ->
    init([Port, [0, 0, 3000, 3000]]);
init([Port, Dimensions]) ->
    {ok, Socket} = gen_udp:open(Port, [list, {active,true}]),
    {ok, #state{socket=Socket, dimensions=Dimensions}}.

%% @private
handle_call({set_world_dimensions, Dim}, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{dimensions=Dim}}.

%% @private
%% @spec handle_cast(Msg, State) -> {noreply, State} | {noreply,
%% State, Timeout} | {stop, Reason, State}
%%
%% @doc Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @spec handle_info(Info, State) -> {noreply, State} | {noreply,
%% State, Timeout} | {stop, Reason, State}
%%
%% @doc Handling all non call/cast messages
handle_info({udp, Socket, IP, InPortNo, Packet},
	    State=#state{socket=Socket, dimensions=Dimensions}) ->
    skirmish_server_client_handler:start({IP, InPortNo}, Dimensions, Packet),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
%% @spec terminate(Reason, State) -> void()
%%
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason.  The return value is ignored.
terminate(_Reason, #state{socket=Socket}) ->
    gen_udp:close(Socket),
    ok.

%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% == Internal functions ==
