-module(echo).
-behaviour(gen_echo).
-export([start/0, start_link/0]).
-export([init/1, echo/2]).

start() ->
    io:fwrite("~w:start()~n", [?MODULE]),
    gen_echo:start(?MODULE, [], []).

start_link() ->
    io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_echo:start(?MODULE, [], []).

%% Callbacks.
init(_Args) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, noState}.
    
echo(Data, State) ->
    io:fwrite("~w:echo(~w)~n", [?MODULE, Data]),
    {string:concat("You said: ", Data), State}.
