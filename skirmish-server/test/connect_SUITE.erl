%%%-------------------------------------------------------------------
%%% File    : connect_SUITE.erl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Story 0: Connect
%%%
%%% Created : 14 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------
-module(connect_SUITE).

-compile(export_all).

-include("ct.hrl").
-include("../include/skirmish_server.hrl").

-define(DEFAULT_PORT, 1657).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    application:start(skirmish_server),
    Config.

end_per_suite(_Config) ->
    application:stop(skirmish_server),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

sequences() -> 
    [].

all() -> 
    [test_valid_connection_requests].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_valid_connection_requests() -> 
    [].

test_valid_connection_requests(_Config) -> 
    {ok, Socket} = gen_udp:open(0, [list,{active, true}]),
    {ok, Localhost} = inet:getaddr("localhost", inet),
    ok = gen_udp:send(Socket, Localhost, ?DEFAULT_PORT,
		      format_handshake("abc", "def")),
    receive
	{udp, Socket, Localhost, Port, "ok" ++ [10, 10]} ->
	    {ok, Port}
    after 1000 ->
	    throw(timeout)
    end,
    ok.

format_handshake(Id, Secret) ->
    io_lib:format("version ~w~n"
		  "id ~s~n"
		  "secret ~s~n"
		  "~n",
		  [?PROTOCOL_VERSION, Id, Secret]).
