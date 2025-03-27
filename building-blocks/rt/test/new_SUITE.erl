-module(new_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test1/1]).
 
all() -> [test1].
 
test1(_Config) ->
    % ok = rt:start("/home/tbmreza/gh/hh200/building-blocks/rt/test/body-json.erltuples").
    ok = rt:start("/home/tbmreza/gh/hh200/building-blocks/rt/test/e2e.erltuples").
