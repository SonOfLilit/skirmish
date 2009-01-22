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
     test_long_random_secret,
     test_huge_secret,
     test_huge_random_secret,
     test_both_long,
     test_id_garbage,
     test_extra_newlines,
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

test_long_random_secret(_Config) ->
    fatal_contains("abcde", rand(287), ""). % a bug appeared at exactly 287

test_huge_secret(_Config) ->
    fatal_contains("abcde", a(4096), "Secret").

%% I spent a very fun night debugging this test, when it looked like this:
%%
%% test_huge_random_secret(_Config) ->
%%     fatal_contains("abcd", rand(4096), "").
%%
%% I'd see, every single time:
%% =ERROR REPORT==== 16-Jan-2009::03:07:13 ===
%% Bad value on output port 'udp_inet'
%%
%% Eventually I grep'd the erlang source code (gotta love open source software,
%% how could we live without it?) and found one source:
%%
%% otp_src_R12B-3/erts/emulator/beam/io.c
%%
%% int erts_write_to_port(Eterm caller_id, Port *p, Eterm list)
%% [...]
%% 	if ((size = io_list_vec_len(list, &vsize, &csize, 
%% 				    ERL_SMALL_IO_BIN_LIMIT,
%% 				    &pvsize, &pcsize)) < 0) {
%% 	    goto bad_value;
%% [...]
%%
%% 	if (r >= 0) {
%% 	    size -= r;
%% 	    if (drv->output) {
%% 		(*drv->output)((ErlDrvData)p->drv_data, buf, size);
%% 	    }
%% 	    erts_free(ERTS_ALC_T_TMP, buf);
%% 	}
%% 	else if (r == -2) {
%% 	    erts_free(ERTS_ALC_T_TMP, buf);
%% 	    goto bad_value;
%% 	}
%% 	else {
%% 	    ASSERT(r == -1); /* Overflow */
%% 	    erts_free(ERTS_ALC_T_TMP, buf);
%% 	    if ((size = io_list_len(list)) < 0) {
%% 		goto bad_value;
%% 	    }
%% [...]
%%  bad_value: 
%%     p->caller = NIL;
%%     {
%% 	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
%% 	erts_dsprintf(dsbufp, "Bad value on output port '%s'\n", p->name);
%% 	erts_send_error_to_logger_nogl(dsbufp);
%% 	return 1;
%%     }
%%
%% This probably means a bug in gen_udp. In the meantime, experiments
%% shows that the chance of getting this error grow with the size of a
%% random packet I try to send:
%%
%%     4096    always
%%      100    one out of four
%%        5    never
%%
%% Therefore skipping this test until I find a workaround seems to be
%% the best course.
%%

test_huge_random_secret(_Config) -> {skip, "gen_udp bug"}. % TODO: find a solution
%    fatal_contains("abcd", rand(4096), "").

test_both_long(_Config) ->
    fatal_contains(a(200), a(1500), "").

test_id_garbage(_Config) ->
    fatal_contains(rand(200), "", "").

test_extra_newlines(_Config) ->
    protocol_error(test_helper:format_handshake("abc", "def\n")).

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
    protocol_error("asdf").

test_wrong_protocol_version(_Config) ->
% TODO: enable when protocol version increases
%    wrong_protocol_version_with(?PROTOCOL_VERSION - 1),
    wrong_protocol_version_with(?PROTOCOL_VERSION + 1).

wrong_protocol_version_with(FakeVersion) ->
    Handshake = test_helper:format_handshake("abc", "def", FakeVersion),
    fatal_contains(Handshake, "version").


%%--------------------------------------------------------------------
%% INTERNAL
%%--------------------------------------------------------------------

a(N) ->
    lists:duplicate(N, "a").

rand(N) ->
    [random:uniform(256) || _ <- lists:seq(1, N)].


protocol_error(Message) ->
    fatal_contains(Message, "Skirmish server").


fatal_contains(Id, Secret, Token) ->
    fatal_contains(test_helper:format_handshake(Id, Secret), Token).

fatal_contains(Message, Token) ->
    Response = test_helper:connect(Message),
    test_helper:close(),
    "fatal " ++ _ = Response,
    true = length(Response) =< 1024,
    case Token of
	[] ->
	    ok;
	_Else ->
	    true = string:str(Response, Token) > 0
    end.


success(Id, Secret) -> 
    test_helper:connect_successfully(Id, Secret),
    test_helper:close().
