-module(rt).

-include_lib("kernel/include/file.hrl").  % #file_info

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
    Ref = erlang:monitor(process, Pid),
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

    true = check_w_permissions(Prog),

    Clients1 = lists:foldl(fun(C, Clients)->
        {Pid, NewClients} = client_for(Clients, C),
        Pid ! {call_1st, C, self()},
        NewClients
    end, #{}, Prog),

    Prog1 = join([], length(Prog)).

    % Clients2 = lists:foldl(fun(C, Clients)->
    %     {Pid, NewClients} = client_for(Clients, C),
    %     Pid ! {call_2nd, C, self()},
    %     NewClients
    % end, Clients1, Prog1),
    %
    % % ??: property: length(Prog1) == length(Prog2)
    % _Prog2 = join([], length(Prog1)),
    %
    % % % ??: check consistency with callstack
    % % property: length(callstack) == length(Prog2)
    % % property: Clients2 contains Clients1
    % ct:print("INFO: sending exit to clients"),
    % send_exit(Clients2).

join(List, 0) -> List;
join(List, N) ->
    receive
        {res, Callable} ->
            join(List ++ [Callable], N - 1)
    after 15000 ->
        ct:print("ERROR: unannotated long response or process")
    end.



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

            {NewC, NewAcc} = case DepsBeenCalled of
                true ->
                    do_call(Acc, C, call_1st);
                _ ->
                    ct:print("INFO: skipped calling on first sweep"),
                    {C, Acc}
            end,
            From ! {res, NewC},
            worker(NewAcc);


        _Unexpected ->
            ct:print("WARN: unexpected worker args"),
            worker(Acc)
    end.

incr_parens_digit(String) ->
    case re:run(String, "(.*)\\((\\d+)\\)$", [{capture, all_but_first, list}]) of
        {match, [Head, DigitStr]} ->
            Digit = list_to_integer(DigitStr),
            Res =
                lists:flatten(
                    io_lib:format(
                        "~s(~p)",
                        [ string:substr(String, 1, length(String) - length(DigitStr) - 2)
                        , Digit + 1
                        ])),
            Res;
        nomatch ->
            Res = lists:flatten(String ++ " (1)"),
            Res
    end.

bump_filename(Input) ->
    H =
        fun H(P, N)->
            Exists = case file:read_file_info(P, []) of
                {error, _} -> false;
                _ ->
                    ct:print("INFO: ~p exists", [P]),
                    true
            end,
            case Exists of
                false ->
                    P;
                true ->
                    Res = incr_parens_digit(P),
                    H(Res, N + 1)
            end
        end,
    H(Input, 1).

% -spec
% do_call(map(), term(), atom()) -> .
% stack test && ~/.cache/rebar3/bin/rebar3 ct -v
do_call(Acc
      , { Deps
        , Name
        , {req, Method, URL, Headers, Payload, Options} = Req
        , {resp, Codes, {output, RespOutputPath, RespOutputMode}} = Resp
        , {err_stack, ErrStack}
        } = C
      , Caller) -> % ct:print("by ~p: ~p", [Caller, C]),

            % case file:write_file("/home/tbmreza/gh/hh200/rt/img.jpg", Body) of
    % https://hexdocs.pm/hackney/hackney.html#request/5
    NewErrs = case catch hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, Code, _Headers, Ref} ->
            % {HasContents, CannotWrite} = case file:read_file_info(RespOutputPath, []) of
            %      {ok, FileInfo} ->
            %         #file_info{size = S, access = Access} = FileInfo,
            %
            %         {S > 0, (Access == read) or (Access == none)}
            % end,


            % ?? overwrite: can you overwrite a file whose permission is not write
            % ??: relevant write permission becomes that of Dir instead of user specified path

            WriteBodyOrInternalErr =  % -> ErrStack
                fun WriteBodyOrInternalErr(P, ErrStack)->
                    {ok, Body} = hackney:body(Ref),
                    case file:write_file(P, Body) of
                        {error, R} ->
                            ErrStack ++ [internal_err__fs];
                        _ ->
                            ct:print("INFO: written file contents of byte_size ~p", [byte_size(Body)]),
                            ErrStack
                    end
                end,

            % F =    
            %     fun F(P)->
            %         IsFresh = false,
            %         {RespOutputPath, IsFresh}
            %     end,


            % fresh:
            % p  ->  {FreshFilename, false = ExistNonZero?}  ->             write_or_internal_err(FreshFilename)


            case RespOutputMode of
                fresh ->
                    % Fresh file name if need be.
                    Bumped = bump_filename(binary_to_list(RespOutputPath)),
                    ct:print("INFO: writing to ~p", [Bumped]),
                    WriteBodyOrInternalErr(Bumped, ErrStack);
                warn ->
                    ct:print("INFO: file_already_exists"),
                    ErrStack;
                error ->
                    % ??: eexist:
                    % This path of execution is presumably very rare because the file_info change must have
                    % happened between static analysis (up until etf writing) and the very above
                    % line of runtime code.
                    %
                    % In this case at this point, the request has already been made and the response from s.u.t
                    % parsed; what's left to be done is marking the whole callable red.
                    ct:print("INFO: marking testcase as FAIL"),
                    ErrStack;
                overwrite ->
                    ct:print("INFO: overwriting ~p", [RespOutputPath]),
                    ErrStack
            end;

            % overwrite:
            % p  ->  ExistNonZero?                           then log("overwriting RespOutputPath"); write_or_internal_err(p)
            %                                                else                                       write_or_internal_err(p)

            % warn:
            % p  ->  ExistNonZero?                           then log("file_already_exists"); write_or_internal_err(p)
            %                                                else                             write_or_internal_err(p)

            % error:
            % p  ->  ExistNonZero?                           then log("marking testcase as FAIL")




            % overwrite | fresh | warn | error
            % overwrite
            % {ok, Body} = hackney:body(Ref),
            % case file:write_file(RespOutputPath, Body) of
            %     {error, R} -> ct:print("Reason: ~p", [R]);
            %     _ -> ct:print("INFO: overwritten file contents of byte_size ~p", [byte_size(Body)])
            % end,

            % % error
            % {ok, Body} = hackney:body(Ref),
            % case file:write_file(RespOutputPath, Body, [exclusive]) of
            %     {error, eexist} ->
            %         ErrStack ++ [user_err_output_mode]; user_err_output_onexist
            %     _ ->
            %         ct:print("INFO: written file contents of byte_size ~p", [byte_size(Body)]),
            %         ErrStack
            % end;

            % fresh
            % {ok, Body} = hackney:body(Ref),
            % fun try_exclusive_write(P)
            %   case file:write_file(P, Body, [exclusive]) of
            %     {error, eexist} -> try_exclusive_write(P ++ 0)
            %     _ -> INFO: written...
            %
            % try_exclusive_write(AbsFilePath),
            % case file:write_file("/home/tbmreza/gh/hh200/rt/img.jpg", Body, [exclusive]) of
            %     {error, eexist} ->
            %         ct:print("Reason: eexist!");
            %     _ ->
            %         ct:print("INFO: written file contents of byte_size ~p", [byte_size(Body)])
            % end,
            % ct:print("INFO: user-specified output file name didn't exist yet; skipped generating fresh name"),

        _ -> ErrStack ++ [internal_err__hackney]
    end,

    NewC =
        { Deps
        , Name
        , {req, Method, URL, Headers, Payload, Options}
        , Resp
        , {err_stack, NewErrs}
        },

    Old = maps:get(callstack, Acc),
    {NewC, maps:put(callstack, Old ++ [Name], Acc)};

do_call(Acc, Callable, Caller) ->
    ct:print("WARN: unexpected Callable"),
    Acc.
