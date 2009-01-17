%%%-------------------------------------------------------------------
%%% File    : skirmish_server_client_handler.erl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Handles a single connected client
%%%
%%% Created : 15 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------
-module(skirmish_server_client_handler).

-behaviour(gen_fsm).

%% API
-export([start/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-include("../include/skirmish_server.hrl").

-define(NEWLINE, 10).
-define(NL, [?NEWLINE]).

-record(state, {ip, port, socket}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start({IP, Port}, Msg) -> ok,Pid} | ignore | {error,Error}
%% Description: Creates a client_handler gen_fsm. Msg is the message
%%              already sent by the client.
%%--------------------------------------------------------------------
start({IP, Port}, Msg) ->
    gen_fsm:start(?MODULE, {{IP, Port}, Msg}, []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init({Addr = {IP, Port}, Msg}) ->
    {ok, Socket} = gen_udp:open(0, [list, {active,true}]),
%    error_logger:info_msg("received ~p from ~p", [Msg, Addr]),
    Resp = response_to_handshake(Msg),
%    error_logger:info_msg("responding ~p", [Resp]),
    ok = gen_udp:send(Socket, IP, Port, Resp),
    {ok, state_name, #state{ip=IP, port=Port, socket=Socket}}.

response_to_handshake(Handshake) ->
    case parse_handshake(Handshake) of
	{ok, _, _} ->
	    ok();
	parse_error ->
	    fatal("You have reached a Skirmish server, and yet you are not a Skirmish client");
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

fatal(Msg) ->
    "fatal " ++ Msg.
ok() ->
    "ok" ++ ?NL ++ ?NL.

validate_id(Id, Secret) ->
    Valid = lists:all(fun is_valid_id_char/1, Id),
    NoNewLines = string:str(Secret, ?NL) == 0,
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
    case catch(?NL = String) of
	?NL -> ok;
	_Else -> throw(parse_error)
    end.

parse_handshake(Handshake) ->
    catch(do_parse_handshake(Handshake)).

do_parse_handshake(Handshake) ->
    {Version, Rest1} = match_line(Handshake, "version "),
    try	match_exactly(?PROTOCOL_VERSION_STR, Version)
    catch parse_error -> throw(wrong_protocol_version)
    end,

    {Id, Rest2} = match_line(Rest1, "id "),
    {Secret, Rest3} = match_line(Rest2, "secret "),
    match_single_newline(Rest3),

    validate_id(Id, Secret).

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%                                                NextState} |
%%                                          {next_state, NextStateName, 
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
