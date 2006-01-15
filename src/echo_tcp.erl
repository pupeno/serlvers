-module(echo_tcp).
-behaviour(gen_server).
-export([start_link/1, stop/0]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([acceptor/1]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% The follwing function is in charge of accept new TCP connections.
acceptor(LSocket) ->
    io:fwrite("~w:acceptor(~w)~n", [?MODULE, LSocket]),
    {ok, Socket} = gen_tcp:accept(LSocket),
    io:fwrite("Socket=~w~n", [Socket]),
    inet:setopts(Socket, [{active, once}]),
    {ok, Pid} = echo:start(), % A good idea might be adding this to a supervisor, although they are all temporary.
    io:fwrite("Pid=~w~n", [Pid]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    acceptor(LSocket).

%% Callbacks.
init(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, [{active, once}]),
    spawn_link(?MODULE, acceptor, [LSocket]),
    {ok, LSocket}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, LSocket) ->
    gen_tcp:close(LSocket), % Close the socket, we are done.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
