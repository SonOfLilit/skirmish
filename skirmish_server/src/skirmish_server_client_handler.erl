%% @author Aur Saraf <sonoflilit@gmail.com>
%%
%% @private
%% @doc Handles a connected client

-module(skirmish_server_client_handler).

-behaviour(gen_fsm).

%% API
-export([start/3]).

%% gen_fsm callbacks
-export([init/1,
	 connected/2,
	 handle_event/3,
         handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include("../include/skirmish_server.hrl").

-define(NEWLINE, 10).

-record(state, {ip, port, socket, dimensions}).


%% == API ==

%% @spec start(Addr, Dimensions, Msg) -> Result
%% Result = {ok,Pid} | ignore | {error,Error}
%% Pid = pid()
%% Error = {already_started,Pid} | term()
%%
%% @doc Start a skirmish_server_client_handler
%%
%% @see init/1
start(Addr, Dimensions, Msg) ->
    gen_fsm:start(?MODULE, [Addr, Dimensions, Msg], []).


%% == gen_fsm callbacks ==

%% @type dimensions() = [integer()]. Has four members, X, Y, W, H
%%
%% @spec init([Arg]) -> {ok, any(), any()} | {stop, any(), any()}
%% Arg = {IP, Port} | Dimensions | Message
%% IP = integer()
%% Port = integer()
%% Dimensions = dimensions()
%% Message = string()
%%
%% @doc Start a skirmish_server_client_handler.
%%
%% `{IP, Port}' refers to remote address to communicate with.
%%
%% `Dimensions' is a list `[X, Y, W, H]' of the world dimensions.
%%
%% `Message' is the initial handshake message sent by the client.
init([{IP, Port}, Dim, Msg]) ->
    {ok, Socket} = gen_udp:open(0, [list, {active,true}]),
    ParseResult = parse_handshake(Msg),
    Resp = response_to_handshake(ParseResult),
    State = #state{ip=IP, port=Port, socket=Socket, dimensions=Dim},
    send(State, Resp),
    case ParseResult of
	{ok, _Id, _Secret} ->
	    {ok, connected, State};
	{error, Error} ->
	    {stop, Error, State}
    end.


%% == State handler functions ==

connected({message, "game\n\n"}, State) ->
    Resp = lists:flatten(
	     io_lib:format("world-corner ~w,~w\nworld-size ~w,~w\n\n",
			   State#state.dimensions)),
    gen_udp:send(State#state.socket, State#state.ip, State#state.port, Resp),
    {next_state, setup_game, State};
connected({message, Req}, State) ->
    error_logger:error_msg("Got illegal request from client: '~p'", [Req]),
    send(State, fatal_parse_error()),
    {stop, protocol_error, State}.

%% @spec
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%                                                NextState} |
%%                                          {next_state, NextStateName, 
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%%
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @spec
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%%
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to
%% handle the event.
handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% @doc Handle non-event messages.
%%
%% In this case, incoming udp packets handled by generating an
%% appropriate message.
handle_info({udp, _Socket, _Host, _Port, Message}, StateName, State) ->
    gen_fsm:send_event(self(), {message, Message}),
    {next_state, StateName, State}.

%% @doc Clean up for shutdown
terminate(_Reason, _StateName, State) ->
    gen_udp:close(State#state.socket),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% == Internal functions ==

%% === UDP ===

%% @spec send(State, Message::string()) -> ok
%%
%% @doc Sends `Message' to the client
send(#state{ip=Ip, port=Port, socket=Socket}, Message) ->
    ok = gen_udp:send(Socket, Ip, Port, Message).

%% === Parsing ===

%% @spec response_to_handshake(ParseResult) -> string()
%%
%% @doc The appropriate response to the client's first message
response_to_handshake(ParseResult) ->
    case ParseResult of
	{ok, _, _} ->
	    ok();
	{error, Error} ->
	    response_to_error(Error)
    end.
response_to_error(Error) ->
    case Error of
	parse_error ->
	    fatal_parse_error();
	wrong_protocol_version ->
	    fatal("Server is running a different version of Skirmish (" ++
		  ?PROTOCOL_VERSION_STR ++ ")");
	id_too_short ->
	    fatal("id too short, must be at least 3 characters");
	id_too_long ->
    	    fatal("id too long, must be at most 16 characters");
	id_illegal_chars ->
	    fatal("id contains illegal characters, must all be letters, numbers or dots");
	secret_too_long ->
	    fatal("Secret too long, must be at most 255 characters");
	secret_illegal_chars ->
	    fatal("Secret may not contain newlines");
	Result ->
	    error_logger:error_msg("Unexpected error during parsing, got ~p", [Result]),
	    fatal("Server Error. Please try again later.")
    end.

fatal_parse_error() ->
    fatal("You have reached a Skirmish server, and yet you are not a Skirmish client").

fatal(Msg) ->
    "fatal " ++ Msg.
ok() ->
    "ok\n\n".

validate_id(Id, Secret) ->
    Valid = lists:all(fun is_valid_id_char/1, Id),
    NoNewLines = string:str(Secret, "\n") == 0,
    if
	not(length(Id) >= 3) ->
	    throw(id_too_short);
	not(length(Id) =< 16) ->
	    throw(id_too_long);
	not Valid ->
	    throw(id_illegal_chars);
	not(length(Secret) =< 255) ->
	    throw(secret_too_long);
	not NoNewLines ->
	    throw(secret_illegal_chars);
	true ->
	    {ok, Id, Secret}
    end.

is_valid_id_char(C) ->
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    (C == $.).

match([H | Message], [H | FieldName]) ->
    match(Message, FieldName);
match(Rest, []) ->
    Rest;
match(_, _) ->
    throw(parse_error).

match_exactly(S1, S2) when is_list(S1), is_list(S2) ->
    case string:equal(S1, S2) of
	false ->
	    throw(parse_error);
	true ->
	    ok
    end.

get_rest_of_line(Buffer) ->
    case lists:splitwith(fun is_not_newline/1, Buffer) of
	{Value, [?NEWLINE | Rest]} ->
	    {Value, Rest};
	_ ->
	    throw(parse_error)
    end.

is_not_newline(C) ->
    C /= ?NEWLINE.

match_line(Message, FieldName) ->
    get_rest_of_line(match(Message, FieldName)).

match_single_newline(String) ->
    case catch("\n" = String) of
	"\n" -> ok;
	_Else -> throw(parse_error)
    end.

parse_handshake(Handshake) ->
    case catch(do_parse_handshake(Handshake)) of
	Ok = {ok, _, _} ->
	    Ok;
	Error ->
	    {error, Error}
    end.

do_parse_handshake(Handshake) ->
    {Version, Rest1} = match_line(Handshake, "version "),
    try	match_exactly(?PROTOCOL_VERSION_STR, Version)
    catch parse_error -> throw(wrong_protocol_version)
    end,

    {Id, Rest2} = match_line(Rest1, "id "),
    {Secret, Rest3} = match_line(Rest2, "secret "),
    match_single_newline(Rest3),

    validate_id(Id, Secret).
