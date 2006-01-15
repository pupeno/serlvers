-module(echo_launcher).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([acceptor/1]).

start_link({tcp, Port}) ->
    io:fwrite("~w:start_link(~w)~n", [?MODULE, {tcp, Port}]),
    gen_server:start_link(?MODULE, {tcp, Port}, []);
start_link({udp, Port}) ->
    io:fwrite("~w:start_link(~w)~n", [?MODULE, {udp, Port}]),
    gen_server:start_link(?MODULE, {udp, Port}, []).

%% The follwing function is in charge of accept new TCP connections.
acceptor(LSocket) ->
    io:fwrite("~w:acceptor(~w)~n", [?MODULE, LSocket]),
    {ok, Socket} = gen_tcp:accept(LSocket),
    inet:setopts(Socket, [{active, once}]),
    {ok, Pid} = echo:start(), % A good idea might be adding this to a supervisor, although they are all temporary.
    ok = gen_tcp:controlling_process(Socket, Pid),
    acceptor(LSocket).

%% Callbacks.
init({tcp, Port}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {tcp, Port}]),
    process_flag(trap_exit, true),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, once}]), % Open the tcp port.
    spawn_link(?MODULE, acceptor, [LSocket]),               % Launch the acceptor.
    {ok, {tcp, LSocket}};
init({udp, Port}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {udp, Port}]),
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(Port, [{active, once}]), % Open the udp socket.
    {ok, Pid} = echo:start_link(),                       % One worker to take care of UDP.
    gen_udp:controlling_process(Socket, Pid),            % Give the UDP port to the worker.
    {ok, {udp, Socket}}.
  
handle_call(_Request, _From, State) ->
    io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

handle_cast(_Request, State) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.
    
terminate(_Reason, {tcp, LSocket}) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, {tcp, LSocket}]),
    gen_tcp:close(LSocket), % Close the socket, we are done.
    ok;
terminate(_Reason, {udp, Socket}) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, {udp, Socket}]),
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.
