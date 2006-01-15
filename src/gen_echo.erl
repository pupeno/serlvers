-module(gen_echo).
-behaviour(gen_server).
-export([start/3, start/4, start_link/3, start_link/4, stop/1]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1}, {echo,2}, {terminate, 2}];
behaviour_info(_) ->
    undefined.

%% API
start(Module, Args, Options) ->
    io:fwrite("~w:start(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start(?MODULE, {Module, Args}, Options).

start(SupName, Module, Args, Options) ->
    io:fwrite("~w:start(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start(SupName, ?MODULE, {Module, Args}, Options).

start_link(Module, Args, Options) ->
    io:fwrite("~w:start_link(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start_link(?MODULE, {Module, Args}, Options).

start_link(SupName, Module, Args, Options) ->
    io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, Options).

stop(Process) ->
    gen_server:handle_cast(Process, stop).

%% Callbacks.
init({Module, Args}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {Module, Args}]),
    process_flag(trap_exit, true),
    {ok, ModState} = Module:init(Args),
    {ok, {Module, ModState}}.

handle_call(_Request, _From, State) ->
    io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

handle_cast(stop, State) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, stop, State]),
    {stop, normal, State};
handle_cast(_Request, State) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

handle_info({udp, Socket, IP, InPortNo, Packet}, {Module, ModState}) -> % Handle UDP packages.
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {udp, Socket, IP, InPortNo, Packet} , {Module, ModState}]),
    {Reply, NewModState} = Module:echo(Packet, ModState), % Generate the reply.
    gen_udp:send(Socket, IP, InPortNo, Reply),            % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]),          % Enable receiving of packages, get the next one.
    {noreply, {Module, NewModState}};
% Handle TCP packages.
handle_info({tcp, Socket, Packet}, {Module, ModState}) -> % Handle TCP packages.
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {tcp, Socket, Packet}, {Module, ModState}]),
    {Reply, NewModState} = Module:echo(Packet, ModState), % Generate the reply.
    gen_tcp:send(Socket, Reply),                          % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]),          % Enable receiving of packages, get the next one.
    {noreply, {Module, NewModState}};
handle_info({tcp_closed, _Socket}, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {tcp_closed, _Socket}, State]),
    {stop, normal, State};
handle_info(_Info, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.
    
terminate(Reason, {Module, ModState}) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, Reason, {Module, ModState}]),
    ok = Module:terminate(Reason, ModState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.
