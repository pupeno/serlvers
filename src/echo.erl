-module(echo).
-behaviour(gen_server).
-export([start/0, start/1, start_link/0, start_link/1, stop/1, echo/2]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    gen_server:start(?MODULE, [], []).

start(SupName) ->
    gen_server:start(SupName, ?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(SupName) ->
    gen_server:start_link(SupName, ?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

echo(Pid, Data) ->
    gen_server:call(Pid, {echo, Data}).

%% Callbacks.
init(_Args) ->
    io:fwrite("I am ~w.~n", [self()]),
    {ok, noState}.
    
handle_call({echo, Data}, _From, State) ->
    {reply, echo(Data), State}; % Generate reply to the echo message.
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Packet}, State) ->     
    io:fwrite("~w:handle_info(~w, ~w) YEAH!~n", [?MODULE, {tcp, Socket, Packet}, State]),
    Reply = echo(Packet),                        % Generate the reply.
    io:fwrite("Reply: ~w.~n", [Reply]),
    gen_tcp:send(Socket, Reply),                 % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]), % Enable receiving of packages, get the next one.
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {tcp_closed, _Socket} , State]),
    {stop, normal, State};    
handle_info(_Info, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Worker function.
echo(Data) ->
    string:concat("You said: ", Data).
    
