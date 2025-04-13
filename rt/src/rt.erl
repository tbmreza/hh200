-module(rt).

-export([
    start/0
  , loop/0
]).

start() ->
    application:ensure_all_started(hackney).

% Callable is called if env.called_list contains callable.deps
loop() ->

    Env = #{called_list => ["login"]},

    Callable = {{deps, []}, "login", get, {been_called, false}, {err_stack, []}},

    case Callable of
        {{deps, Deps}, Name, Method, BeenCalled, ErrStack} ->
            Fulfilled = lists:all(fun(Dep)->
                lists:member(Dep, maps:get(called_list, Env))
            end, Deps),

            if Fulfilled ->
                do_call(Callable);
            true ->
                ct:print("INFO: unfulfilled callable deps")
            end;
        _ -> ct:print("unexpected callable")
    end,

    ok.


-spec
do_call(term()) -> term().
do_call({Deps, Name, Method, _BeenCalled, {err_stack, ErrStack}}) ->
    % URL = <<"https://friendpaste.com">>,
    URL = <<"http://localhost:9999/p">>,
    Headers = [],
    Payload = <<>>,
    Options = [],

    Errs = case hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, _Rest} -> [];
        _ -> [internal_err_hackney]
    end,
    {Deps, Name, Method, {been_called, true}, {err_stack, Errs ++ ErrStack}}.
