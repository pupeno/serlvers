-module(echo).
-behaviour(gen_echo).
-export([start/0, start/1, start_link/0, start_link/1, stop/1]).
-export([init/1, echo/2, terminate/2]).

start() ->
    io:fwrite("~w:start()~n", [?MODULE]),
    gen_echo:start(?MODULE, [], []).

start(SupName) ->
    io:fwrite("~w:start()~n", [?MODULE]),
    gen_echo:start(SupName, ?MODULE, [], []).

start_link() ->
    io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_echo:start_link(?MODULE, [], []).

start_link(SupName) ->
    io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_echo:start_link(SupName, ?MODULE, [], []).

stop(Process) ->
    gen_echo:stop(Process).

%% Callbacks.
init(_Args) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, noState}.
    
echo(Data, State) ->
    io:fwrite("~w:echo(~w)~n", [?MODULE, Data]),
    {string:concat("You said: ", Data), State}.

terminate(Reason, State) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, Reason, State]),
    ok.
