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
    {ok, noState}.
    
handle_call({echo, Data}, _From, State) ->
    {reply, string:concat("You said: ", Data), State}; % Generate reply to the echo message.
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
