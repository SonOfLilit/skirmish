%%%-------------------------------------------------------------------
%%% File    : requestgame_SUITE.erl
%%% Author  : Aur Saraf <aursaraf@Mac-2.local>
%%% Description : Story 1: Trivial game negotiation
%%%
%%% Created : 22 Jan 2009 by Aur Saraf <aursaraf@Mac-2.local>
%%%-------------------------------------------------------------------
-module(requestgame_SUITE).

-compile(export_all).

-include("ct.hrl").
-include("../include/skirmish_server.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    %% TODO: google what is the path problem
    true = code:add_path("/Users/aursaraf/src/skirmish/skirmish_server/ebin"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    test_helper:close(),
    ok = application:stop(skirmish_server),
    ok.

sequences() -> 
    [].

all() ->
    [test_valid].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_valid(_Config) ->
    ok = application:start(skirmish_server),
    test_helper:connect_successfully("myid", ""),
    is_response(0, 0, 3000, 3000).

%%--------------------------------------------------------------------
%% INTERNAL
%%--------------------------------------------------------------------

is_response(X, Y, W, H) ->
    Response = test_helper:request_game(),
    Response = format_response(X, Y, W, H).

format_response(X, Y, W, H) ->
    lists:flatten(
      io_lib:format("world-corner ~w,~w\nworld-size ~w,~w\n\n", [X, Y, W, H])).
