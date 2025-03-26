-module(rt).

-export([fmt_url/2, start/1]).
-export([boot/1, interval/1, interp/2]).
-export([insert_at_position/3]).

interval(Lines) ->
    timer:apply_repeatedly(2000, rt, interp, [#{}, Lines]),
    % interp(#{}, Lines),
    % interp(#{}, Lines),
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

start(Path) ->
    application:start(inets),  % exception: {noproc, _}
    application:start(ssl),
    {ok, Binary} = file:read_file(Path),
    Content = unicode:characters_to_list(Binary),
    Lines = lists:filter(
              fun(W)-> W =/= [] end,
              string:split(Content, "\n", all)),
    interp(#{}, Lines),
    ok.

interp(Env, Lines) ->
    try lists:foldl(
        fun(E, AccEnv)->
            {Result, NewEnv} = eval(E, AccEnv),  % ??: eval(Tokens, AccEnv), % scanner FFI to erlang tuples
            ct:print("~p~n", [Result]),
            io:format("~p~n", [Result]),
            NewEnv
        end, Env, Lines)
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

% Triggering ops: http, req_size_lim, {
content_json() -> "application/json".
headers() -> [{"Content-Type", content_json()}].
% }

% Env keys: callable, response
eval(Expr, Env) ->  % -> IO NewEnv
    {ok, Tokens, _} = erl_scan:string(Expr ++ "."),
    {ok, ParsedExpr} = erl_parse:parse_exprs(Tokens),

    % Triggering ops: http, req_size_lim, {
    ContentType = "application/json",
    Headers = [{"Content-Type", ContentType}],
    % }

    case catch erl_eval:exprs(ParsedExpr, erl_eval:new_bindings()) of
        {value, {Method,Url}, _NewBindings} when Method == get
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


        {value, {repeat, Times, Str}, _NewBindings} ->
            % R = lists:duplicate(Times, Str),
            % ct:print("~p", [R]),
            % {R, Env};
            {lists:duplicate(Times, Str), Env};


        {value, {capture, Pairs}, _NewBindings} ->
            {ok, update_env(Env, Pairs)};


        {value, {forget, Keys}, _NewBindings} ->
            {ok, maps:without(Keys, Env)};


        {value, {var, Key}, _NewBindings} ->
            {maps:get(Key, Env), Env};


        % Set callable body.
        {value, {json, Obj}, _NewBindings} ->
            CallableWithBody = maps:put(body, Obj, maps:get(callable, Env)),
            NewEnv = maps:put(callable, CallableWithBody, Env),

            {maps:get(callable, NewEnv), NewEnv};


        {value, {http, ExpectCode}, _NewBindings} ->
            % #{method := SMethod,
            %   url := SUrl,
            %   body := SBody
            %  } = maps:get(callable, Env),
            %
            % R = case SMethod of
            %     Method when Method == post orelse Method == patch ->
            %         % ContentType = "application/json",
            %         % Headers = [{"Content-Type", ContentType}],
            %         case SBody of
            %             {} ->
            %                 ct:print("INFO: post/patch without content!"),
            %                 {SUrl, [], [], []};
            %
            %             {path, P} ->
            %                 case catch file:read_file(P) of
            %                     {ok, Content} ->
            %                         {SUrl, Headers, ContentType, Content};
            %                     _ ->
            %                         ct:print("WARN: failed reading file at path=~p", [P]),
            %                         {SUrl, [], [], []}
            %                 end;
            %
            %             % When SBody is a list of chars (string):
            %             _ when is_list(SBody) ->
            %                 {SUrl, Headers, ContentType, json:format(#{username => <<"admin">>, password => <<"1234">>})}
            %                 ;
            %
            %             % When SBody is a map:
            %             % _ when not is_list(SBody) ->
            %             _ ->
            %                 ct:print("INFO: ??"),
            %                 {SUrl, Headers, ContentType, json:format(SBody)}
            %         end;
            %     _HeadOptionsGet ->
            %         {SUrl, []}
            % end,
            % io_req(Env, ExpectCode, SMethod, R);

            io_req2(Env, ExpectCode);


        % req_size_lim initially sends ExpectMinBytes, expecting ExpectCode.
        % If it results in unmatching codes, return false.
        %
        % Otherwise, try again with (ExpectMinBytes div 10 + 1 ~= 10%) size
        % (i.e. `ls -l` file size, not `du -h` block size) increase.
        % If it results in unmatching codes, return the last size that succeeded.
        %
        % The iteration stops when the size gets to 2x of ExpectMinBytes.
        {value, {req_size_lim, ExpectMinBytes, ExpectCode}, _NewBindings} ->  % {false | Bytes, NewEnv}
            #{
              % method := SMethod,
              % url := SUrl,
              body := SBody
             } = maps:get(callable, Env),

            case SBody of
                {mut, Path} -> ct:print("INFO: modifying provided file at Path=~p", [Path]);

                {path, Path} ->
                    ct:print("WARN: unmet invariant of mutable file path, using provided file once."),

                    FileMeetsExpectMinBytes =
                        case catch file:read_file_info(Path) of
                            {ok, Tup} ->
                                [file_info, Size | _] = tuple_to_list(Tup),
                                Size >= ExpectMinBytes;
                            _ -> false
                        end,

                    {Code, NewEnv} = io_req2r(Env, ExpectCode),

                    if FileMeetsExpectMinBytes and (Code == ExpectCode) ->
                        {ExpectMinBytes, Env};
                    true ->
                        {false, Env}
                    end;

                _ ->
                    ct:print("ERROR: unmet invariant of mutable file path"),
                    {false, Env}
            end;



            % stash:
            % #{method := SMethod,
            %   url := SUrl,
            %   body := SBody
            %  } = maps:get(callable, Env),
            %
            % R = case SMethod of
            %     Method when Method == post orelse Method == patch ->
            %         % ContentType = "application/json",
            %         % Headers = [{"Content-Type", ContentType}],
            %         case SBody of
            %             {} ->
            %                 ct:print("INFO: post/patch without content!"),
            %                 {SUrl, [], [], []};
            %
            %             % When SBody is a list of chars (string):
            %             _ when is_list(SBody) ->
            %                 {SUrl, Headers, ContentType, json:format(#{username => <<"admin">>, password => <<"1234">>})}
            %                 ;
            %
            %             % When SBody is a map:
            %             % _ when not is_list(SBody) ->
            %             _ ->
            %                 {SUrl, Headers, ContentType, json:format(SBody)}
            %         end;
            %     _HeadOptionsGet ->
            %         {SUrl, []}
            % end,
            %
            % {Code, NewEnv} = io_req(Env, ExpectCode, SMethod, R),
            % % case of
            % %   413 TooLarge or 504 50x Gateway or Error -> {false, NewEnv}
            % %   _ -> H(ExpectMinBytes, Stride)
            % case Code of
            %     % 401 ->
            %     Code when Code == 401 orelse Code == 504 ->
            %         Bytes = 109,
            %         ct:print("~p~n", [Bytes]),
            %         {Bytes, NewEnv};
            %     _ -> {todo, NewEnv}
            % end;


        {value, Result, NewBindings} ->
            NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
            {Result, NewEnv};


        _V ->
            [{var,_Anno,Name}] = ParsedExpr,

            Result = maps:get(Name, Env),
            {Result, Env}
    end.


% H(AccBytes, Stride) ->
%     case io_req_of_size of
%       nok -> AccBytes;
%       _ ->   
%           insert_at_position("/home/tbmreza/gh/hh200/building-blocks/rt/asset.json",
%               lists:duplicate(Stride, "a"),
%               9)
%           H(AccBytes + Stride, Stride)
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


io_req2r(Env, ExpectCode) ->  % -> {Code, NewEnv}
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
                    ct:print("INFO: ??"),
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

io_req2(Env, ExpectCode) ->  % -> {Code, NewEnv}
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
                    ct:print("INFO: ??"),
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

io_req(Env, ExpectCode, Method, R) ->  % -> {Code, NewEnv}
    case catch httpc:request(Method, R, [], []) of
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
