-module(interpreter).
-export([start/0, eval/1, eval/2]).

% Start the interpreter with an empty environment
start() ->
    application:start(inets),
    io:format("Simple Erlang Interpreter~n"),
    % io:format("Enter expressions or 'quit' to exit~n"),
    loop(#{}).

% Main interpreter loop
loop(Env) ->
    Input = io:get_line("> "),
    case string:trim(Input) of
        % "quit" -> 
        %     io:format("Goodbye!~n");
        "" ->
            loop(Env);
        Expr ->
            try
                {Result, NewEnv} = eval(Expr, Env),
                io:format("~p~n", [Result]),
                loop(NewEnv)
            catch
                error:Reason ->
                    io:format("Error: ~p~n", [Reason]),
                    loop(Env)
            end
    end.

% Evaluate an expression with the given environment

% mod Types
is_http_method(Method) ->
    Method == get orelse
    Method == post orelse
    Method == put orelse
    Method == delete orelse
    Method == patch orelse
    Method == options orelse
    Method == head.

eval(Expr, Env) ->
    {ok, Tokens, _} = erl_scan:string(Expr ++ "."),
    {ok, ParsedExpr} = erl_parse:parse_exprs(Tokens),

    % Evaling ExprList might contain bind statements.
    case catch erl_eval:exprs(ParsedExpr, erl_eval:new_bindings()) of
        % {value, {get,Url}, NewBindings} ->

        % {value, {get,Url}, _} ->
        % {value, {Method,Url}, _} when is_http_method(Method) ->
        % {value, {Method,Url}, _} when lists:member(Method, [get, post, put, delete, patch, options, head]) ->
        {value, {Method,Url}, _} when Method == get ->
            % {ok, {{_Version, _Code, _ReasonPhrase}, _Headers, _Body}} =
            %     httpc:request(get, {Url, []}, [], []),

            % NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
            % {result_here, NewEnv};

            % {result_here, Env};
            % {result_here, maps:put(callable, #{method => get, url => Url}, Env)};
            {result_here, maps:put(callable, #{method => Method, url => Url}, Env)};

        {value, {http,_ExpectCode}, NewBindings} ->
            % {Method,Url} = env_callable(Env),
            #{method := Method, url := Url} =
                maps:get(callable, Env, #{method => get, url => "http://localhost:9999/unreachable"}),

            {ok, {{_Version, _Code, _ReasonPhrase}, _Headers, _Body}} =
                % httpc:request(get, {"http://localhost:9999/new", []}, [], []),
                httpc:request(Method, {Url, []}, [], []),
            NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
            {result_here, NewEnv};

        {value, Result, NewBindings} ->
            NewEnv = update_env(Env, erl_eval:bindings(NewBindings)),
            {Result, NewEnv};

        V ->
            {maybe_var_in_Env, Env}
    end.



% Evaluate an expression with empty environment

eval(Expr) ->
    eval(Expr, #{}).

% Update environment with new bindings
update_env(Env, Bindings) ->
    lists:foldl(
        fun({Var, Val}, Acc) -> 
            maps:put(Var, Val, Acc)
        end,
        Env,
        Bindings
    ).
