-module(rt).

-export([fmt_url/2, start/1]).

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



fmt_url(_Env, Url) ->
    case Url of
        {fmt, Template, Exprs} ->
            % ??: what ast is enabled after this; case E is term eval else ident
            % Exprs:
            % ["polate"]  ok
            % ["dir", "child"]  ok
            % [{var,user_id}]
            % eval until it's list of string/digit literals.

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

% Env keys: callable, response
eval(Expr, Env) ->  % -> IO NewEnv
    {ok, Tokens, _} = erl_scan:string(Expr ++ "."),
    {ok, ParsedExpr} = erl_parse:parse_exprs(Tokens),

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


        % Invariants: callable,
        {value, {http, ExpectCode}, _NewBindings} ->
            #{method := SMethod,
              url := SUrl,
              body := SBody
             } = maps:get(callable, Env),

            R = case SMethod of
                Method when Method == post orelse Method == patch ->
                    case SBody of
                        {} ->
                            % ??: log info: post/patch without content,
                            {SUrl, [], [], []};
                        Obj ->
                            ContentType = "application/json",
                            Headers = [{"Content-Type", ContentType}],
                            {SUrl, Headers, ContentType, json:format(SBody)}
                    end;
                _ ->
                    {SUrl, []}
            end,

            io_req(Env, ExpectCode, SMethod, R);


        {value, Result, NewBindings} ->
            NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
            {Result, NewEnv};


        _V ->
            [{var,_Anno,Name}] = ParsedExpr,

            Result = maps:get(Name, Env),
            {Result, Env}
    end.



update_env(Env, Bindings) -> lists:foldl(
    fun({K, V}, Acc)-> maps:put(K, V, Acc) end,
    Env,
    Bindings).


io_req(Env, ExpectCode, Method, R) ->  % -> {Result, NewEnv}
    case catch httpc:request(Method, R, [], []) of
        {ok, {{_Version, Code, _ReasonPhrase}, Headers, Body}} ->
            Result = Code == ExpectCode,
            {Result,
             maps:put(response,
                      #{
                        headers => Headers,
                        body => Body
                       },
                      Env)};
        V ->
            {V, Env}

    end.
