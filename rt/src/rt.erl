-module(rt).

-export([
    start/0
  , worker/1
]).

start() ->
	% clients<name, client_id>
    % ManagedPs = maps:put(Name, Pid, #{}).
    application:ensure_all_started(hackney),
    Pid1 = spawn(?MODULE, worker, [#{callstack => []}]),
    Pid1.


-spec
add_client(map(), binary()) -> map().
add_client(Clients, CallableName) ->
    Pid = spawn(?MODULE, worker, [init_acc()]),
    maps:put(CallableName, Pid, Clients).


-spec
find_client(map(), list(binary())) -> pid().
find_client(Clients, CallableDeps) ->
    H = fun H(Deps)->
        [Dep | Rest] = Deps,
        case catch maps:get(Dep, Clients) of
            {badkey, _} -> H(Rest);
            V -> V
        end
    end,
    H(CallableDeps).

-spec
send_exit(map()) -> true.
send_exit(Clients) ->
    lists:foreach(fun(Pid)->
        case is_process_alive(Pid) of
          true ->
              exit(Pid, kill);
          _ ->
              true
        end
    end, maps:values(Clients)).

    % Env = #{callstack => ["login"]},
% Start haskell-analyzed .etf program.
% -spec
% start_etf(binary()) -> any().
start_etf(Path) ->
    {ok, B} = file:read_file(Path),
    Prog = binary_to_term(B),
    true = some_empty_thenprefs(Prog),
    true = check_w_permissions(Prog),

    % case Prog of
    %     [_ | _] ->
    %         % If callable has deps, find clients.dep
    %         % walk(#{}, Prog);
    %     _ ->
    %         % If then-prefixes is empty, spawn new client.
    %         ct:print("WARN: empty program")
    % end,

    Callable =
        {{deps, []}, "login",
         {req, post, <<"http://localhost:9999/p">>, [], <<>>, []},
         {been_called, false}, {err_stack, []}},

    Clients = add_client(#{}, "login"),

    find_client(Clients, callable_deps(Callable)) ! {do_call, Callable},

    ok.

-spec
some_empty_thenprefs(list(term())) -> boolean().
some_empty_thenprefs(Prog) -> true.

-spec
check_w_permissions(list(term())) -> boolean().
check_w_permissions(Prog) -> true.



-spec
init_acc() -> map().
init_acc() -> #{callstack => []}.

-spec
callable_deps(term()) -> list(binary()).
callable_deps({{deps, Deps}, _, _, _, _}) -> Deps.

-spec
callable_name(term()) -> binary().
callable_name({_, Name, _, _, _}) -> Name.

-spec
worker(map()) -> map().
worker(Acc) ->
    % io:format("~p: Got hello from ~p~n", [self(), Callable]),
    receive
        {done} -> Acc;
        % Callable is called if Acc.callstack contains callable.deps

        {do_call, Callable} ->
            Called = maps:get(callstack, Acc),
            IsReady = lists:member(callable_name(Callable), Called),
            case IsReady of
                true ->
                    {_, Name, _, _, _} = do_call(Callable),
                    Updated = maps:put(callstack, Called ++ Name, Acc),
                    worker(Updated);
                false ->
                    ct:print("INFO: callable not ready"),
                    worker(Acc)
            end;


        Unexpected ->
            worker(Acc)
    end.

-spec
do_call(term()) -> term().
do_call({Deps, Name, {req, Method, URL, Headers, Payload, Options}, _BeenCalled, {err_stack, ErrStack}}) ->
    Errs = case hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, _Rest} -> [];
        _ -> [internal_err_hackney]
    end,
    {Deps, Name, Method, {been_called, true}, {err_stack, Errs ++ ErrStack}}.
