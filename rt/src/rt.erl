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


% -spec
% add_client(map(), binary()) -> .
add_client(Clients, CallableName) ->
    Pid = spawn(?MODULE, worker, [init_acc()]),
    {Pid, maps:put(CallableName, Pid, Clients)}.


-spec
find_dep_client_or_add(map(), term()) -> {pid(), map()}.
find_dep_client_or_add(Clients, Callable) ->
    Deps = callable_deps(Callable),
    H = fun H(Deps)->
        case Deps of
            [] ->
                add_client(Clients, callable_name(Callable));
            [Dep | Rest] ->
                case maps:get(Dep, Clients, default) of
                    default ->
                        case Rest of
                            [] -> add_client(Clients, callable_name(Callable));
                            _ ->  H(Rest)
                        end;
                    Pid -> {Pid, Clients}
                end
        end
    end,
    H(Deps).

-spec
client_for(map(), term()) -> {pid(), map()}.
client_for(Clients, Callable) ->
    Name = callable_name(Callable),
    case maps:get(Name, Clients, default) of
        default ->
            find_dep_client_or_add(Clients, Callable);
        Pid ->
            {Pid, Clients}
    end.

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
    [C, D] = Prog,

    % {Pida, Clts} = client_for(#{<<"login">> => 9}, C),
    % ct:print("got: ~p", [Clts]),

    {SweptProg, {Clients}} = lists:mapfoldl(fun(C, {Clients})->
        {Pid, NewClients} = client_for(Clients, C),
        Pid ! {maybe_call, C, self()},

        receive
            {res, Callable} ->
                % Callable
                ct:print("received: ~p", [Callable])
        after 2000 ->
            ct:print("Timed out waiting for message~n")
        end,
        {C, {NewClients}}
    end, {#{}}, Prog),

    % first sweep: call if all deps in Acc.callstack      | regardless Callable.err_stack
    % map Prog                 worker maybe_call       -> Prog (err_stack)

    % second sweep: call if Callable.err_stack not empty  | regardless Acc.callstack
    % foldl Prog, case Callable.err_stack worker call  -> Acc


    % % ??: check consistency with callstack
    ct:print("INFO: sending exit to clients"),
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
        {result} -> Acc;

        % Second sweep.
        {call, Callable} -> worker(Acc);

        {maybe_call, Callable, From} ->
            Deps = callable_deps(Callable),
            Callstack = maps:get(callstack, Acc),
            DepsCalled = lists:all(fun(Dep)-> lists:member(Dep, Callstack) end, Deps),

            NewAcc = case DepsCalled of
                true -> do_call(Acc, Callable);
                _ ->
                    ct:print("INFO: skipped calling on first sweep"),
                    Acc
            end,
            ct:print("From=~p", [From]),
            ct:print("Callable=~p", [Callable]),
            From ! {res, Callable},
            worker(NewAcc);


        Unexpected ->
            ct:print("WARN: unexpected worker args"),
            worker(Acc)
    end.

-spec
do_call(map(), term()) -> map().
do_call(Acc, Callable) ->
    % {Deps, Name, {req, Method, URL, Headers, Payload, Options}, {err_stack, ErrStack}} = Callable,
    % Errs = case hackney:request(Method, URL, Headers, Payload, Options) of
    %     {ok, _Rest} -> [];
    %     _ -> [internal_err_hackney]
    % end,
    % {Deps, Name, Method, {err_stack, Errs ++ ErrStack}}.
    Old = maps:get(callstack, Acc),
    maps:put(callstack, Old ++ callable_name(Callable), Acc).
