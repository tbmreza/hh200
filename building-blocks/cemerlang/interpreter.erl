-module(interpreter).
% -export([dbg/0, start/1, eval/2]).
-export([dbg/0, reg/0, start/1, eval/2, fmt_url/2, r/0]).

r() ->
    % URL with query parameters
    % Url = "http://example.com/api/resource?id=123",
    Url = "http://localhost:9999/yo",

    % JSON body
    JsonBody = "{\"name\":\"John\",\"age\":30}",

    % Set content type for JSON
    ContentType = "application/json",
    Headers = [{"Content-Type", ContentType}],

    % Make the POST request
    Request = {Url, Headers, ContentType, JsonBody},
    httpc:request(post, Request, [], []).
    % false.

reg() -> start("basic-get.erltuples").
dbg() -> start("debug.erltuples").

start(Path) ->
    application:start(inets),  % exception: {noproc, _}
    application:start(ssl),
    {ok, Binary} = file:read_file(Path),
    Content = unicode:characters_to_list(Binary),
    Lines = lists:filter(
              fun(W)-> W =/= [] end,
              string:split(Content, "\n", all)),
    interp(#{}, Lines).

interp(Env, Lines) ->
    try lists:foldl(
        fun(E, AccEnv)->
            {Result, NewEnv} = eval(E, AccEnv),
            io:format("~p~n", [Result]),
            NewEnv
        end, Env, Lines)
    % catch error:Reason ->
    %     io:format("Error interpreting line: ~p~n", [Reason])
    % end.
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


fmt_url(Env, Url) ->
    case Url of
        {fmt, Template, Exprs} ->
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
            % ??: case E is term eval else ident
            io_lib:format(Template, Args);
        _ -> Url
    end.


% Env keys: callable, response
eval(Expr, Env) ->
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
            {Env, maps:put(callable,
                           #{
                             method => Method,
                             url => fmt_url(Env, Url)
                            },
                           Env)};


        {value, {capture, Pairs}, _NewBindings} ->
            {ok, update_env(Env, Pairs)};


        {value, {forget, Keys}, _NewBindings} ->
            {ok, maps:without(Keys, Env)};


        {value, {var, Key}, _NewBindings} ->
            {maps:get(Key, Env), Env};


        % Set callable body.
        {value, {json, Str}, _NewBindings} ->
            % ?? static-check: httpc doesn't allow json body for get requests.
            % {unimplemented, Env};
            CallableWithBody = maps:put(body, Str, maps:get(callable, Env)),
            NewEnv = maps:put(callable, CallableWithBody, Env),

            {maps:get(callable, NewEnv), NewEnv};


        {value, {http, ExpectCode}, _NewBindings} ->
            #{method := Method, url := Url} =
                maps:get(callable, Env, #{method => get, url => "http://localhost:9999/unreachableP"}),

            ContentType = "application/json",
            Headers = [{"Content-Type", ContentType}],
            JsonBody = maps:get(callable_body,
                                Env,
                                "{\"name\":\"John\",\"age\":31}"),

            % case catch httpc:request(post, {Url, Headers, ContentType, JsonBody}, [], []) of
            % ??: POST, PATCH not ok
            case catch httpc:request(Method, {Url, []}, [], []) of
                {ok, {{_Version, Code, _ReasonPhrase}, Headers, Body}} ->
                    Result = Code == ExpectCode,
                    {Result, maps:put(response,
                                      #{
                                        headers => Headers,
                                        body => Body
                                       },
                                      Env)};
                V ->
                    % {hh200_panicked, Env}
                    {V, Env}

            end;


        {value, Result, NewBindings} ->
            NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
            {Result, NewEnv};


        _V ->
            [{var,_Anno,Name}] = ParsedExpr,

            Result = maps:get(Name, Env),
            {Result, Env}
    end.



% eval(Expr) -> eval(Expr, #{}).

update_env(Env, Bindings) -> lists:foldl(
    fun({K, V}, Acc)-> maps:put(K, V, Acc) end,
    Env,
    Bindings).
