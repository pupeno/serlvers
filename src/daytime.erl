-module(daytime).
-behaviour(gen_daytime).
-export([start/0, start/1, start_link/0, start_link/1, stop/1]).
-export([init/1, daytime/1, terminate/2]).

start() ->
    io:fwrite("~w:start()~n", [?MODULE]),
    gen_daytime:start(?MODULE, [], []).

start(SupName) ->
    io:fwrite("~w:start(~w)~n", [?MODULE, SupName]),
    gen_daytime:start(SupName, ?MODULE, [], []).

start_link() ->
    io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_daytime:start_link(?MODULE, [], []).

start_link(SupName) ->
    io:fwrite("~w:start_link(~w)~n", [?MODULE, SupName]),
    gen_daytime:start_link(SupName, ?MODULE, [], []).

stop(Process) ->
    gen_daytime:stop(Process).

%% Callbacks.
init(_Args) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, []}.
    
daytime(State) ->
    io:fwrite("~w:daytime()~n", [?MODULE]),
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = calendar:universal_time(),
    DayTime = lists:flatten(
		io_lib:format("~w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w+0000~n", 
			      [Year, Month, Day, Hours, Minutes, Seconds])),
    {DayTime, State}.

terminate(Reason, State) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, Reason, State]),
    ok.
