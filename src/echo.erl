-module(echo).
-behaviour(gen_server).
-export([start/0, start/1, start_link/0, start_link/1]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    gen_server:start(?MODULE, [], []).

start(SupName) ->
    gen_server:start(SupName, ?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(SupName) ->
    gen_server:start_link(SupName, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%
%%%% Callbacks %%%%

init(_Args) ->
    {ok, noState}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

% Handle UDP packages.
handle_info({udp, Socket, IP, InPortNo, Packet}, State) -> % Socket and Socket are and must be the same, isn't Erlang awesome ?
    Reply = echo(Packet),                        % Generate the reply.
    gen_udp:send(Socket, IP, InPortNo, Reply),   % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]), % Enable receiving of packages, get the next one.
    {noreply, State};
% Handle TCP packages.
handle_info({tcp, Socket, Packet}, State) ->
    Reply = echo(Packet),                        % Generate the reply.
    gen_tcp:send(Socket, Reply),                 % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]), % Enable receiving of packages, get the next one.
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Worker function.
echo(Data) ->
    string:concat("You said: ", Data).
    
