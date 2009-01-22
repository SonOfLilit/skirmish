%%%-------------------------------------------------------------------
%%% File    : test_helper.hrl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Helper functions for Skirmish Server unit tests
%%%
%%% Created : 22 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------

-module(test_helper).

-export([connect/1,
	 format_handshake/2,
	 format_handshake/3]).

-include("../include/skirmish_server.hrl").

-define(DEFAULT_PORT, 1657).

connect(Message) ->
    {ok, Socket} = gen_udp:open(0, [list,{active, true},{sndbuf,8196}]),
    {ok, Localhost} = inet:getaddr("localhost", inet),
    ok = gen_udp:send(Socket, Localhost, ?DEFAULT_PORT, Message),
    receive
	{udp, Socket, Host, Port, Response} ->
	    Response
    after 1000 ->
	    throw(timeout)
    end.


format_handshake(Id, Secret) ->
    format_handshake(Id, Secret, ?PROTOCOL_VERSION).

format_handshake(Id, Secret, Version) ->
    "version " ++ io_lib:write(Version) ++ "\n"
	"id " ++ Id ++ "\n"
	"secret " ++ Secret ++ "\n"
	"\n".
