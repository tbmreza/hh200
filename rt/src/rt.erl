-module(rt).

-export(
  [ start/0
  , start_etf/1
  , worker/1
]).

start() ->
    ok.


-spec
add_client(map(), binary()) -> {pid(), map()}.
add_client(Clients, CallableName) ->
    Pid = spawn(?MODULE, worker, [init_acc()]),
    {Pid, maps:put(CallableName, Pid, Clients)}.


-spec
find_dep_client_or_add(map(), term()) -> {pid(), map()}.
find_dep_client_or_add(Clients, Callable) ->
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
    H(callable_deps(Callable)).

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

    true = some_empty_thenprefs(Prog),
    true = check_w_permissions(Prog),

    Clients1 = lists:foldl(fun(C, Clients)->
        {Pid, NewClients} = client_for(Clients, C),
        Pid ! {call_1st, C, self()},
        NewClients
    end, #{}, Prog),

    Prog1 = join([], length(Prog)),

    % first sweep:  call if all deps in Acc.callstack

    % second sweep: call if name not in callstack OR Callable.err_stack not empty

    Clients2 = lists:foldl(fun(C, Clients)->
        {Pid, NewClients} = client_for(Clients, C),
        Pid ! {call_2nd, C, self()},
        NewClients
    end, Clients1, Prog1),

    % ??: property: length(Prog1) == length(Prog2)
    _Prog2 = join([], length(Prog1)),

    % % ??: check consistency with callstack
    % property: length(callstack) == length(Prog2)
    % property: Clients2 contains Clients1
    ct:print("INFO: sending exit to clients"),
    send_exit(Clients2).

join(List, 0) -> List;
join(List, N) ->
    receive
        {res, Callable} ->
            join(List ++ [Callable], N - 1)
    after 15000 ->
        ct:print("ERROR: unannotated long response or process")
    end.


-spec
some_empty_thenprefs(list(term())) -> boolean().
some_empty_thenprefs(_Prog) -> true.

-spec
check_w_permissions(list(term())) -> boolean().
check_w_permissions(_Prog) -> true.

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
callable_errstack(term()) -> list().
callable_errstack({_, _, _, _, Errs}) -> Errs.

-spec
worker(map()) -> map().
worker(Acc) ->
    application:ensure_all_started(hackney),
    receive
        {call_2nd, C, From} ->
            Callstack = maps:get(callstack, Acc),
            Name = callable_name(C),
            ErrStackNotEmpty = case callable_errstack(C) of
                [] -> false;
                _ -> true
            end,
            HasChance = not lists:member(Name, Callstack) or ErrStackNotEmpty,
            case HasChance of
                true -> do_call(Acc, C, call_2nd);
                _ ->
                    ct:print("INFO: skipped calling on first sweep"),
                    Acc
            end,
            From ! {res, C},
            worker(Acc);

        {call_1st, C, From} ->
            Callstack = maps:get(callstack, Acc),
            Deps = callable_deps(C),
            DepsBeenCalled = lists:all(fun(Dep)-> lists:member(Dep, Callstack) end, Deps),

            NewAcc = case DepsBeenCalled of
                true -> do_call(Acc, C, call_1st);
                _ ->
                    ct:print("INFO: skipped calling on first sweep"),
                    Acc
            end,
            From ! {res, C},
            worker(NewAcc);


        _Unexpected ->
            ct:print("WARN: unexpected worker args"),
            worker(Acc)
    end.

-spec
do_call(map(), term(), atom()) -> map().
do_call(Acc, {Deps, Name, {req, Method, URL, Headers, Payload, Options}, Resp, {err_stack, ErrStack}}, Caller) ->
    ct:print("do_call by ~p", [Caller]),
    Errs = case catch hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, _Rest} -> [];
        _ -> [internal_err_hackney]
    end,

    Old = maps:get(callstack, Acc),
    maps:put(callstack, Old ++ [Name], Acc);

do_call(Acc, Callable, Caller) ->
    ct:print("WARN: unexpected Callable"),
    Acc.
