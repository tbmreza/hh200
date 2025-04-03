-module(rt).

-export([start_/1]).
-export([fmt_url/2]).
% -export([start/1]).
-export([boot/1]).
-export([interval/1]).
% -export([interp/2]).
-export([insert_at_position/3]).

-export([par/1]).

% ??: read up .app.src looking to create sub rt module
par(Path) ->
    application:ensure_all_started(hackney),
    Method = get,
    % URL = <<"https://friendpaste.com">>,
    URL = <<"http://localhost:9999">>,
    Headers = [],
    Payload = <<>>,
    Options = [],
    {ok, Binary} = file:read_file(Path),
    ct:print("file content: ~p", [Binary]),

    Content = unicode:characters_to_list(Binary),
    Lines = lists:filter(
              fun(W)-> W =/= [] end,
              string:split(Content, "\n", all)),

    interp_(#{}, Binary),

    % {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(Method, URL,
    %                                                         Headers, Payload,
    %                                                         Options).

    ok.

interval(Lines) ->
    timer:apply_repeatedly(2000, rt, interp, [#{}, Lines]),
    ok.

% {interval, 3000, [{post, url}...{http, 200}]}

boot(Path) ->  % -> Lines
    application:start(inets),  % exception: {noproc, _}
    application:start(ssl),
    {ok, Binary} = file:read_file(Path),
    Content = unicode:characters_to_list(Binary),
    lists:filter(
      fun(W)-> W =/= [] end,
      string:split(Content, "\n", all)).

% Start haskell-analyzed .etf program.
start_(Path) ->
    {ok, B} = file:read_file(Path)
  , Prog = binary_to_term(B)

  , case Prog of
        [_ | _] ->
            ct:print("NOT MT"),
            application:start(inets),
            application:start(ssl),
            interp_(#{}, Prog);
        _ -> ct:print("WARN: empty program")
    end

  , ct:print("done").

    % case Prog of
    %     [_ | _] ->
    %         application:start(inets),
    %         application:start(ssl),
    %         interp_(#{}, Prog);
    %     _ -> ct:print("WARN: empty program")
    % end.


% start(ErltuplesPath) ->
%     application:start(inets),  % exception: {noproc, _}
%     application:start(ssl),
%     {ok, Binary} = file:read_file(ErltuplesPath),
%     Content = unicode:characters_to_list(Binary),
%     Lines = lists:filter(
%               fun(W)-> W =/= [] end,
%               string:split(Content, "\n", all)),
%     interp(#{}, Lines),
%     ok.

interp_(Env, Prog) ->
    try lists:foldl(
        fun(E, AccEnv)->
            {Result, NewEnv} = eval_(E, AccEnv),  % ??: eval(Tokens, AccEnv), % scanner FFI to erlang tuples
            ct:print("~p~n", [Result]),
            io:format("~p~n", [Result]),
            NewEnv
        end, Env, Prog)
    catch 
        error:Reason:Stacktrace ->
            % Extract line number from stacktrace
            LineInfo = case lists:filtermap(
                fun({_, Module, _, Info}) ->
                    case proplists:get_value(line, Info) of
                        undefined -> false;
                        LineNum -> {true, LineNum}
                    end
                end, 
                Stacktrace
            ) of
                [FirstLine | _] -> FirstLine;
                [] -> unknown
            end,
            
            % Detailed error logging
            io:format("Error interpreting code:~n"
                      "  Reason: ~p~n"
                      "  Line: ~p~n"
                      "  Stacktrace: ~p~n", 
                      [Reason, LineInfo, Stacktrace]),
            
            % Optionally, you might want to reraise or handle differently
            Env
    end.

-spec fmt_url(integer(), integer()) -> integer().  % ??
fmt_url(_Env, Url) ->
    case Url of
        {fmt, Template, Exprs} ->
            % ??: eval until it's list of string/digit literals.

            Args = Exprs,
            % [Z0|_] = Exprs,
            % io:format("z0: ~p~n", [Z0]),
            % R = io_lib:format("~p",[Z0]),
            % Z = eval(lists:flatten(R), Env),

            % Eval arguments for format template without updating Env.
            % Args = lists:map(fun(E)-> eval(E, Env) end, Exprs),
            io_lib:format(Template, Args);
        _ -> Url
    end.

% Triggering ops: http, probe_valid_size, {
content_json() -> "application/json".
headers() -> [{"Content-Type", content_json()}].
% }

% Env keys: callable, response
eval_(Expr, Env) ->  % -> IO NewEnv
    % Triggering ops: http, probe_valid_size, {
    ContentType = "application/json",
    Headers = [{"Content-Type", ContentType}],
    % }

    case Expr of
        % see 3 responses in server log after interpreting {par, 3, [{post, url}...{http, 200}]}
        {par, N, Prog} ->
            ct:print("PAR EVAL ??"),
            {ok, Env};

        {Method, Url} when Method == get
                    orelse Method == post
                    orelse Method == put
                    orelse Method == delete
                    orelse Method == patch
                    orelse Method == options
                    orelse Method == head ->
            {Env,
             maps:put(callable,
                      #{
                        method => Method,
                        url => fmt_url(Env, Url),
                        % Empty tuples as nil; use empty map %{} of empty json object.
                        body => {}
                       },
                      Env)};


        {repeat, Times, Str} ->
            % R = lists:duplicate(Times, Str),
            % ct:print("~p", [R]),
            % {R, Env};
            {lists:duplicate(Times, Str), Env};


        {capture, Pairs} ->
            {ok, update_env(Env, Pairs)};


        {forget, Keys} ->
            {ok, maps:without(Keys, Env)};


        {var, Key} ->
            {maps:get(Key, Env), Env};


        % Set callable body.
        {json, Obj} ->
            CallableWithBody = maps:put(body, Obj, maps:get(callable, Env)),
            NewEnv = maps:put(callable, CallableWithBody, Env),

            {maps:get(callable, NewEnv), NewEnv};


        {http, ExpectCode} ->
            io_req(Env, ExpectCode)
            ;


        % probe_valid_size initially sends ExpectMinBytes, expecting ExpectCode.
        % If even that results in unmatching codes, return false.
        %
        % Otherwise, try again with (ExpectMinBytes div 10 + 1 ~= 10%) size (i.e.
        % `ls -l` file size, not `du -h` block size) increase.
        % At one point it results in unmatching codes, return the last size that succeeded.
        % (Or capped at double of ExpectMinBytes).
        {probe_valid_size, ExpectMinBytes, ExpectCode} ->  % {false | Bytes, NewEnv}
            #{
              % method := SMethod,
              % url := SUrl,
              body := SBody
             } = maps:get(callable, Env),

            case SBody of
                {mut, Path, Position} ->
                    Tenth = ExpectMinBytes div 10 + 1,

                    ct:print("INFO: modifying provided file at Path=~p", [Path]),
                    LastSize = meet_expect_min_bytes(ExpectMinBytes, Path, Position, Tenth),

                    {Code, NewEnv} = io_req(Env, ExpectCode),
                    if Code =/= ExpectCode ->
                        {false, Env};

                    true ->
                        H = fun H(CodesDidMatch, LastSize, E)-> case CodesDidMatch of
                            false ->
                                {min(2 * ExpectMinBytes, LastSize), E};
                            _ ->
                                insert_at_position(Path, lists:duplicate(Tenth, "k"), Position),  % ??: assert sizes before after insertion
                                {Code, NewEnv} = io_req(E, ExpectCode),
                                H(Code == ExpectCode, file_size_or_panic(Path), NewEnv)
                            end
                        end,
                        H(true, LastSize, Env)
                    end;

                {path, Path} ->
                    ct:print("WARN: unmet invariant of mutable file path, using provided file once."),

                    FileMeetsExpectMinBytes =
                        case catch file:read_file_info(Path) of
                            {ok, Tup} ->
                                [file_info, Size | _] = tuple_to_list(Tup),
                                Size >= ExpectMinBytes;
                            _ -> false
                        end,

                    {Code, NewEnv} = io_req(Env, ExpectCode),

                    if FileMeetsExpectMinBytes and (Code == ExpectCode) ->
                        {ExpectMinBytes, Env};
                    true ->
                        {false, Env}
                    end;

                _ ->
                    ct:print("ERROR: unmet invariant of mutable file path"),
                    {false, Env}
            end
            % ;


        % {value, Result, NewBindings} ->
        %     NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
        %     {Result, NewEnv};


        % _V ->
        %     [{var,_Anno,Name}] = ParsedExpr,
        %
        %     Result = maps:get(Name, Env),
        %     {Result, Env}
    end.

% eval(Expr, Env) ->  % -> IO NewEnv
%     {ok, Tokens, _} = erl_scan:string(Expr ++ "."),
%     {ok, ParsedExpr} = erl_parse:parse_exprs(Tokens),
%
%     % Triggering ops: http, probe_valid_size, {
%     ContentType = "application/json",
%     Headers = [{"Content-Type", ContentType}],
%     % }
%
%     case catch erl_eval:exprs(ParsedExpr, erl_eval:new_bindings()) of
%         % see 3 responses in server log after interpreting {par, 3, [{post, url}...{http, 200}]}
%         {value, {par, N, Prog}, _NewBindings} ->
%             ct:print("PAR EVAL ??"),
%             {ok, Env};
%
%         {value, {Method,Url}, _NewBindings} when Method == get
%                                           orelse Method == post
%                                           orelse Method == put
%                                           orelse Method == delete
%                                           orelse Method == patch
%                                           orelse Method == options
%                                           orelse Method == head ->
%             {Env,
%              maps:put(callable,
%                       #{
%                         method => Method,
%                         url => fmt_url(Env, Url),
%                         % Empty tuples as nil; use empty map %{} of empty json object.
%                         body => {}
%                        },
%                       Env)};
%
%
%         {value, {repeat, Times, Str}, _NewBindings} ->
%             % R = lists:duplicate(Times, Str),
%             % ct:print("~p", [R]),
%             % {R, Env};
%             {lists:duplicate(Times, Str), Env};
%
%
%         {value, {capture, Pairs}, _NewBindings} ->
%             {ok, update_env(Env, Pairs)};
%
%
%         {value, {forget, Keys}, _NewBindings} ->
%             {ok, maps:without(Keys, Env)};
%
%
%         {value, {var, Key}, _NewBindings} ->
%             {maps:get(Key, Env), Env};
%
%
%         % Set callable body.
%         {value, {json, Obj}, _NewBindings} ->
%             CallableWithBody = maps:put(body, Obj, maps:get(callable, Env)),
%             NewEnv = maps:put(callable, CallableWithBody, Env),
%
%             {maps:get(callable, NewEnv), NewEnv};
%
%
%         {value, {http, ExpectCode}, _NewBindings} ->
%             io_req(Env, ExpectCode);
%
%
%         % probe_valid_size initially sends ExpectMinBytes, expecting ExpectCode.
%         % If even that results in unmatching codes, return false.
%         %
%         % Otherwise, try again with (ExpectMinBytes div 10 + 1 ~= 10%) size (i.e.
%         % `ls -l` file size, not `du -h` block size) increase.
%         % At one point it results in unmatching codes, return the last size that succeeded.
%         % (Or capped at double of ExpectMinBytes).
%         {value, {probe_valid_size, ExpectMinBytes, ExpectCode}, _NewBindings} ->  % {false | Bytes, NewEnv}
%             #{
%               % method := SMethod,
%               % url := SUrl,
%               body := SBody
%              } = maps:get(callable, Env),
%
%             case SBody of
%                 {mut, Path, Position} ->
%                     Tenth = ExpectMinBytes div 10 + 1,
%
%                     ct:print("INFO: modifying provided file at Path=~p", [Path]),
%                     LastSize = meet_expect_min_bytes(ExpectMinBytes, Path, Position, Tenth),
%
%                     {Code, NewEnv} = io_req(Env, ExpectCode),
%                     if Code =/= ExpectCode ->
%                         {false, Env};
%
%                     true ->
%                         H = fun H(CodesDidMatch, LastSize, E)-> case CodesDidMatch of
%                             false ->
%                                 {min(2 * ExpectMinBytes, LastSize), E};
%                             _ ->
%                                 insert_at_position(Path, lists:duplicate(Tenth, "k"), Position),  % ??: assert sizes before after insertion
%                                 {Code, NewEnv} = io_req(E, ExpectCode),
%                                 H(Code == ExpectCode, file_size_or_panic(Path), NewEnv)
%                             end
%                         end,
%                         H(true, LastSize, Env)
%                     end;
%
%                 {path, Path} ->
%                     ct:print("WARN: unmet invariant of mutable file path, using provided file once."),
%
%                     FileMeetsExpectMinBytes =
%                         case catch file:read_file_info(Path) of
%                             {ok, Tup} ->
%                                 [file_info, Size | _] = tuple_to_list(Tup),
%                                 Size >= ExpectMinBytes;
%                             _ -> false
%                         end,
%
%                     {Code, NewEnv} = io_req(Env, ExpectCode),
%
%                     if FileMeetsExpectMinBytes and (Code == ExpectCode) ->
%                         {ExpectMinBytes, Env};
%                     true ->
%                         {false, Env}
%                     end;
%
%                 _ ->
%                     ct:print("ERROR: unmet invariant of mutable file path"),
%                     {false, Env}
%             end;
%
%
%         {value, Result, NewBindings} ->
%             NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
%             {Result, NewEnv};
%
%
%         _V ->
%             [{var,_Anno,Name}] = ParsedExpr,
%
%             Result = maps:get(Name, Env),
%             {Result, Env}
%     end.


% ??: par or c ffi
insert_at_position(Filename, InsertString, Position) ->
    % Read the file content
    {ok, Binary} = file:read_file(Filename),

    % Convert binary to a list (string)
    Content = binary_to_list(Binary),

    % Insert the string at the specified position
    {Before, After} = lists:split(Position, Content),
    NewContent = Before ++ InsertString ++ After,

    % Write the modified content back to the file
    {ok, FileRef} = file:open(Filename, [write]),
    file:write(FileRef, NewContent),
    file:close(FileRef).


update_env(Env, Bindings) -> lists:foldl(
    fun({K, V}, Acc)-> maps:put(K, V, Acc) end,
    Env,
    Bindings).


% par_io_req(Env, ExpectCode) ->

io_req(Env, ExpectCode) ->  % -> {Code, NewEnv}
    #{method := SMethod,
      url := SUrl,
      body := SBody
     } = maps:get(callable, Env),

    R = case SMethod of
        Method when Method == post orelse Method == patch ->
            case SBody of
                {} ->
                    ct:print("INFO: post/patch without content!"),
                    {SUrl, [], [], []};

                {mut, P, _} ->
                    case catch file:read_file(P) of
                        {ok, Content} ->
                            {SUrl, headers(), content_json(), Content};
                        _ ->
                            ct:print("WARN: failed reading file at path=~p", [P]),
                            {SUrl, [], [], []}
                    end;

                {path, P} ->
                    case catch file:read_file(P) of
                        {ok, Content} ->
                            {SUrl, headers(), content_json(), Content};
                        _ ->
                            ct:print("WARN: failed reading file at path=~p", [P]),
                            {SUrl, [], [], []}
                    end;

                % When SBody is a list of chars (string):
                _ when is_list(SBody) ->
                    {SUrl, headers(), content_json(), json:format(#{username => <<"admin">>, password => <<"1234">>})}
                    ;

                % When SBody is a map:
                % _ when not is_list(SBody) ->
                _ ->
                    ct:print("INFO: ???"),
                    {SUrl, headers(), content_json(), json:format(SBody)}
            end;
        _HeadOptionsGet ->
            {SUrl, []}
    end,
    case catch httpc:request(SMethod, R, [], []) of
        {ok, {{_Version, Code, _ReasonPhrase}, Headers, Body}} ->
            if Code =/= ExpectCode ->
                ct:print("expected: ~p, got: ~p~n", [ExpectCode, Code]); true -> nil
            end,
            {Code,
             maps:put(response,
                      #{
                        headers => Headers,
                        body => Body
                       },
                      Env)};
        V ->
            ct:print("got: ~p~n", [V]),
            {false, Env}

    end.


file_size_or_panic(Path) ->
    {ok, Tup} = file:read_file_info(Path),
    [file_info, Size | _] = tuple_to_list(Tup),
    Size.



% Test via `hh200 input.hhs` where
% ```hhs
% POST http://localhost:9999/413.php
% probe_valid_size {ExpectMinBytes} {ExpectCode}
% ```
meet_expect_min_bytes(ExpectMinBytes, Path, Position, Stride) ->  % -> IO FileSizeInBytes
    H = fun H(Size)-> case Size of
        Size when Size >= ExpectMinBytes -> Size;
        _ ->
            insert_at_position(Path, lists:duplicate(Stride, "k"), Position),
            H(file_size_or_panic(Path))
        end
    end,
    H(file_size_or_panic(Path)).
