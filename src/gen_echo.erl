-module(gen_echo).
-behaviour(gen_server).
-export([start/3, start/4, start_link/3, start_link/4]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1}, {echo,2}];
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

%echo(Pid, Data) ->
%    gen_server:call(Pid, {echo, Data}).

%% Callbacks.
init({Module, Args}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {Module, Args}]),
    {ok, ModState} = Module:init(Args),
    {ok, {Module, ModState}}.

handle_call(_Request, _From, State) ->
    io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

handle_cast(_Request, State) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

% Handle UDP packages.
handle_info({udp, Socket, IP, InPortNo, Packet}, {Module, ModState}) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {udp, Socket, IP, InPortNo, Packet} , {Module, ModState}]),
    {Reply, NewModState} = Module:echo(Packet, ModState), % Generate the reply.
    gen_udp:send(Socket, IP, InPortNo, Reply),            % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]),          % Enable receiving of packages, get the next one.
    {noreply, {Module, NewModState}};
% Handle TCP packages.
handle_info({tcp, Socket, Packet}, {Module, ModState}) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {tcp, Socket, Packet}, {Module, ModState}]),
    {Reply, NewModState} = Module:echo(Packet, ModState), % Generate the reply.
    gen_tcp:send(Socket, Reply),                          % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]),          % Enable receiving of packages, get the next one.
    {noreply, {Module, NewModState}};
handle_info(_Info, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.
    
terminate(_Reason, _State) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.
    
