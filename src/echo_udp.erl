-module(echo_udp).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Callbacks.
init(_Args) ->
    {ok, Socket} = gen_udp:open(10007, [{active, once}]), % Open the udp socket.
    {ok, _} = echo:start_link({local, udp_echo}),   % One worker to take care of UDP.
    {ok, {Socket}}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

% Receive an UDP message.
handle_info({udp, Socket, IP, InPortNo, Packet}, {Socket}) -> % Socket and Socket are and must be the same, isn't Erlang awesome ?
    Reply = echo:echo(udp_echo, Packet),         % Generate the reply.
    gen_udp:send(Socket, IP, InPortNo, Reply),   % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]), % Enable receiving of packages, get the next one.
    {noreply, {Socket}};

handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, {Socket}) ->
    gen_udp:close(Socket), % Close the socket, we are done.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
