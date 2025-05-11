-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
    all/0
  , test1/1
]).
all() -> [test1].

% ??: how can we reuse ct for summarizing hh200 script results
test1(_Config) ->
    % ct:print("S=~p", [S]),
    rt:start_etf("/home/tbmreza/gh/hh200/hh200/temp.etf"),
    1 = 1.

% test1(_Config) ->
%     T = [{{deps, []}, "login",
%           {req, post, <<"http://localhost:9999/p">>, [], <<>>, []},
%           {resp, 200, {output, <<"outfile">>}},
%           {been_called, false}, {err_stack, []}}],
%
%     {ok, A} = file:read_file("/home/tbmreza/gh/hh200/hh200/unittest.etf"),
%     {ok, B} = file:read_file("/home/tbmreza/gh/hh200/hh200/bunittest.etf"),
%     ProgA = binary_to_term(A),
%     ProgB = binary_to_term(B),
%     ProgA = ProgB.


% -export([test1/1]).
% -export([test2/1]).
% -export([test3/1]).
%  
% all() -> [test1,test2,test3].
%  
% test1(_Config) ->
%     Env = #{}, Url = {fmt, "http://localhost:9999/~s", ["polate"]},
%     Result = rt:fmt_url(Env, Url),
%     Str = io_lib:format("http://localhost:9999/~s", ["polate"]),
%
%     Str = Result.
%
%  
% test2(_Config) ->
%     Env = #{}, Url = {fmt, "http://localhost:9999/~s/~s", ["dir", "child"]},
%     Result = rt:fmt_url(Env, Url),
%     Str = io_lib:format("http://localhost:9999/~s/~s", ["dir", "child"]),
%
%     Str = Result.
%
%
% test3(_Config) ->
%     ok = rt:start_("/home/tbmreza/gh/hh200/hh200/unittest.etf").
