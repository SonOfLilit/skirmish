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
    %% TODO: google what is the path problem
    true = code:add_path("/Users/aursaraf/src/skirmish/skirmish_server/ebin"),
    ok = application:start(skirmish_server),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(skirmish_server).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

sequences() -> 
    [].

all() -> 
    [test_too_short_id,
     test_empty_id,
     test_too_long_id,
     test_very_long_id,
     test_too_long_secret,
     test_huge_random_secret,
     test_both_long,
     test_id_garbage,
     test_simplest_valid_case,
     test_average_valid_case,
     test_longest_allowed_id_and_secret,
     test_most_contrived_valid_case,
     test_wrong_protocol_version,
     test_protocol_error].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Invalid id/secret
%%--------------------------------------------------------------------

test_too_short_id(_Config) ->
    fatal_contains("ab", "", "too short").

test_empty_id(_Config) ->
    fatal_contains("", "asd", "too short").

test_empty_both(_Config) ->
    fatal_contains("", "", "too short").

test_too_long_id(_Config) ->
    fatal_contains(a(17), "", "too long").

test_very_long_id(_Config) ->
    fatal_contains(a(1024), "", "too long").

test_too_long_secret(_Config) ->
    fatal_contains("abcde", a(1024), "too long").

test_huge_random_secret(_Config) ->
    {skip, "Until I find a way to format it"}.
%    fatal_contains("abcd", rand(4096), "seecret").

test_both_long(_Config) ->
    fatal_contains(a(200), a(1500), "").

test_id_garbage(_Config) ->
    fatal_contains(rand(200), "", "").

%%--------------------------------------------------------------------
%% Valid cases
%%--------------------------------------------------------------------

test_simplest_valid_case(_Config) ->
    success("abc", "").

test_average_valid_case(_Config) ->
    success("abcdefg", "hijklmnop").

test_longest_allowed_id_and_secret(_Config) ->
    success(a(16), a(255)).

test_most_contrived_valid_case(_Config) ->
    success("aA1.aA1.aA1.aA1.", lists:seq(1, 9) ++ lists:seq(11, 255)).

%%--------------------------------------------------------------------
%% Network issues
%%--------------------------------------------------------------------

test_protocol_error(_Config) ->
    Error = fatal_contains("asdf", "Skirmish server").

test_wrong_protocol_version(_Config) ->
% TODO: enable when protocol version increases
%    test_wrong_protocol_version_with(?PROTOCOL_VERSION - 1),
    test_wrong_protocol_version_with(?PROTOCOL_VERSION + 1).

test_wrong_protocol_version_with(FakeVersion) ->
    Handshake = format_handshake("abc", "def", FakeVersion),
    fatal_contains(Handshake, "version").



%%--------------------------------------------------------------------
%% INTERNAL
%%--------------------------------------------------------------------

a(N) ->
    lists:duplicate(N, "a").

rand(N) ->
    [random:uniform(256) || _ <- lists:seq(1, N)].

fatal_contains(Message, Token) ->
    Response = connect(Message),
    "fatal " ++ _ = Response,
    true = length(Response) =< 1024,
    true = string:str(Response, Token) > 0.

fatal_contains(Id, Secret, Token) ->
    fatal_contains(format_handshake(Id, Secret), Token).

success(Id, Secret) -> 
    "ok" ++ [10, 10] = connect(format_handshake(Id, Secret)).

connect(Message) ->
    {ok, Socket} = gen_udp:open(0, [list,{active, true}]),
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
    io_lib:format("version ~w~n"
		  "id ~s~n"
		  "secret ~s~n"
		  "~n",
		  [Version, Id, Secret]).
