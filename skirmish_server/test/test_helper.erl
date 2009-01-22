%%%-------------------------------------------------------------------
%%% File    : test_helper.hrl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Helper functions for Skirmish Server unit tests
%%%
%%% Created : 22 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------

-module(test_helper).

-export([connect/1,
	 connect_successfully/2,
	 close/0,
	 request_game/0,
	 format_handshake/2,
	 format_handshake/3]).

% internal
-export([do_connect/1]).

-include("../include/skirmish_server.hrl").

-define(DEFAULT_PORT, 1657).

format_handshake(Id, Secret) ->
    format_handshake(Id, Secret, ?PROTOCOL_VERSION).

format_handshake(Id, Secret, Version) ->
    "version " ++ io_lib:write(Version) ++ "\n"
	"id " ++ Id ++ "\n"
	"secret " ++ Secret ++ "\n"
	"\n".


connect_successfully(Id, Secret) ->
    "ok\n\n" = connect(format_handshake(Id, Secret)).

connect(Message) ->
    Child = spawn(?MODULE, do_connect, [self()]),
    true = register(test_client, Child),
    test_client ! {send, Message},
    await_response().

request_game() ->
    test_client ! {send, "game\n\n"},
    await_response().

close() ->
    test_client ! close,
    true = unregister(test_client).


do_connect(Parent) ->
    {ok, Socket} = gen_udp:open(0, [list,{active, true},{sndbuf,8196}]),
    {ok, Localhost} = inet:getaddr("localhost", inet),
    loop(Parent, Socket, Localhost, ?DEFAULT_PORT).

loop(Parent, Socket, Host, Port) ->
    receive
	{send, Message} ->
	    ok = gen_udp:send(Socket, Host, Port, Message),
	    loop(Parent, Socket, Host, Port);
	{udp, Socket, Host, NewPort, Response} ->
	    Parent ! {response, Response},
	    %% According to protocol, NewPort /= Port happens once and
	    %% from then on communication is on NewPort
	    loop(Parent, Socket, Host, NewPort);
	close ->
	    gen_udp:close(Socket)
    end.


await_response() ->
    receive
	{response, Response} ->
	    Response
    after 1000 ->
	    close(),
	    throw(timeout)
    end.

