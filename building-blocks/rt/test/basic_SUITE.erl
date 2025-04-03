-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test1/1]).
-export([test2/1]).
-export([test3/1]).
 
all() -> [test1,test2,test3].
 
test1(_Config) ->
    Env = #{}, Url = {fmt, "http://localhost:9999/~s", ["polate"]},
    Result = rt:fmt_url(Env, Url),
    Str = io_lib:format("http://localhost:9999/~s", ["polate"]),

    Str = Result.

 
test2(_Config) ->
    Env = #{}, Url = {fmt, "http://localhost:9999/~s/~s", ["dir", "child"]},
    Result = rt:fmt_url(Env, Url),
    Str = io_lib:format("http://localhost:9999/~s/~s", ["dir", "child"]),

    Str = Result.


test3(_Config) ->
    ok = rt:start_("/home/tbmreza/gh/hh200/hh200/unittest.etf").
