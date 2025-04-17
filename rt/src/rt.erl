-module(rt).

-export([
    start/0
  , start_etf/1
  , worker/1
]).

% true = some_empty_thenprefs(Prog),
% true = check_w_permissions(Prog),
% Env = #{callstack => ["login"]},
% io:format("~p: Got hello from ~p~n", [self(), Callable]),
% clients<name, client_id>
start() ->
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
send_exit(map()) -> ok.
send_exit(Clients) ->
    lists:foreach(fun(Pid)->
        case is_process_alive(Pid) of
          true ->
              exit(Pid, kill);
          _ ->
              true
        end
    end, maps:values(Clients)).

% Start haskell-analyzed .etf program.
-spec
start_etf(binary()) -> any().
start_etf(Path) ->
    {ok, B} = file:read_file(Path),
    Prog = binary_to_term(B),

    % PICKUP
    % first sweep: call if deps are called
    lists:foreach(fun(C)->
        Deps = callable_deps(C),
        ct:print("~p", [Deps])
    end, Prog),

    % second sweep: call if err_stack not empty

    % Clients = add_client(#{}, "login"),
    %
    % FinalAcc = lists:foreach(fun(Callable)->
    %     Pid = case callable_deps(Callable) of
    %         [] -> maps:get(callable_name(Callable), Clients);
    %         _ -> find_client(Clients, callable_deps(Callable))
    %     end,
    %     Pid ! {do_call, Callable}
    % end, Prog_),
    %
    % FinalAcc,
    %
    % % ??: check consistency with callstack
    send_exit(#{}).

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
callable_deps({{deps, Deps}, _, _, _, _, _}) -> Deps.

-spec
callable_name(term()) -> binary().
callable_name({_, Name, _, _, _, _}) -> Name.

-spec
worker(map()) -> map().
worker(Acc) ->
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
