-module(echo_udp).
-behaviour(gen_server).
-export([start_link/1, stop/0]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Callbacks.
init(Port) ->
    {ok, Socket} = gen_udp:open(Port, [{active, once}]), % Open the udp socket.
    {ok, Pid} = echo:start_link({local, udp_echo}),      % One worker to take care of UDP.
    gen_udp:controlling_process(Socket, Pid),            % Give the UDP port to the worker.
    {ok, Socket}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, Socket) ->
    gen_udp:close(Socket), % Close the socket, we are done.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
