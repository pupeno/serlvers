%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of Serlvers.
%%
%% Serlvers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% Serlvers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Serlvers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking Serlvers statically or dynamically with other modules is making a combined work based on Serlvers. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of Serlvers give you permission to combine Serlvers program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Serlvers and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%% @doc Launcher launches tcp and udp serviceses based on the behaviours of Serlvers.
%% <p>It basically handles opening the sockets and waiting for data to latter send it to the serlvers.</p>
%% <p>Whenever you see the word "serlver" used as a noun it refeers to a module implementing one of the Serlvers behaviours like {@link gen_echo}, {@link gen_daytime}, etc.</p>
%% <p>For tcp services a new worker is spawned on each connection and that worker handles the connection untill termination. The message {connected, Socket} where connected is an atom and Socket the connected socket is sent to the worker upon connection.</p>
%% <p>For udp services only one worker is spawned which will handle all datagrams.</p>
%% @see gen_echo.
%% @see gen_chargen.
%% @see gen_daytime.
%% @see gen_time.
%% @since 0.1.0

%% @type transport() = tcp | udp

-module(launcher).
-behaviour(gen_server).
-export([start/3, start/4, start_link/3, start_link/4, stop/1]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([acceptor/3]).

%% @doc Launch an unnamed serlver.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start/4
%% @see start_link/3
%% @since 0.1.0
%% @spec (Module::atom(), Transport::transport(), Port::integer()) -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Reason}
start(Module, Transport, Port) ->
    %%io:fwrite("~w:start(~w, ~w, ~w)~n", [?MODULE, Module, Transport, Port]),
    gen_server:start(?MODULE, {Module, Transport, Port}, []).

%% @doc Launch a named serlver.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start/3
%% @see start_link/4
%% @since 0.1.0
%% @spec (Name, Module::atom(), Transport::transport(), Port::integer()) -> Result
%%   Name = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Reason}
start(SupName, Module, Transport, Port) ->
    %%io:fwrite("~w:start(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Transport, Port]),
    gen_server:start(SupName, ?MODULE, {Module, Transport, Port}, []).

%% @doc Launch an unamed serlver and link to it.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start_link/4
%% @see start/3
%% @since 0.1.0
%% @spec (Module::atom(), Transport::transport(), Port::integer()) -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Reason}
start_link(Module, Transport, Port) ->
    %%io:fwrite("~w:start_link(~w, ~w, ~w)~n", [?MODULE, Module, Transport, Port]),
    gen_server:start_link(?MODULE, {Module, Transport, Port}, []).

%% @doc Launch a named serlver and link to it.
%% <p>Name is passed to gen_server:start_link/4 as is.</p>
%% <p>Module is the name of a module implementing one of the service behaviours (gen_echo, gen_chargen, etc).</p>
%% <p>The Port port will be opened using Trasport (either tcp or udp) as the transport. Not all services can work with all transports.</p>
%% @see start_link/3
%% @see start/4
%% @since 0.1.0
%% @spec (Name, Module::atom(), Transport::transport(), Port::integer()) -> Result
%%   Name = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Reason}
start_link(SupName, Module, Transport, Port) ->
    %%io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Transport, Port]),
    gen_server:start_link(SupName, ?MODULE, {Module, Transport, Port}, []).

%% @doc Stops a running process identified by Name.
%% <p>This function is intended to help debugging while developing serlvers. When a server goes in production, launcher should be run by a supervisor that takes care of starting and stoping.</p>
%% @see start/3
%% @see start/4
%% @see start_link/3
%% @see start_link/4
%% @since 0.1.0
%% @spec (Name) -> ok
%%   Name = atom() | {local, atom()} | {global, atom()}
stop({_Scope, Name}) ->
    %%io:fwrite("~w:stop(~w)~n", [?MODULE, {_Scope, Name}]),
    gen_server:cast(Name, stop);
stop(Name) ->
    %%io:fwrite("~w:stop(~w)~n", [?MODULE, Name]),
    gen_server:cast(Name, stop).

%% @doc The follwing function is in charge of accepting new TCP connections.
%% <p>It is <i>spawned</i> from {@link init} when called for a tcp server.</p>
%% @private Only called from {@link init}.
%% @since 0.1.0
acceptor(tcp, Module, LSocket) ->
    io:fwrite("~w:acceptor(~w, ~w, ~w)~n", [?MODULE, tcp, Module, LSocket]),
    case gen_tcp:accept(LSocket) of                        % Wait for an incomming connection.
	{ok, Socket} ->
	    {ok, Pid} = Module:start(),                    % Run a worker.
	    ok = gen_tcp:controlling_process(Socket, Pid), % Let the worker control this connection.
	    Pid ! {connected, Socket},                     % Worker, wake up, you have to work (this is for the cases where upon connection, the worker has to do something, like daytime and time, unlike echo).
	    ok = inet:setopts(LSocket, [{active, once}]),  % Enable accepting one connection.
	    acceptor(tcp, Module, LSocket);                % Wait for the next connection.
	{error, Reason} ->
	    {error, Reason}
    end;
acceptor(udp, Module, LSocket) ->
    io:fwrite("~w:acceptor(~w, ~w, ~w)~n", [?MODULE, udp, Module, LSocket]),
    receive                                               % Wait for a message.
        {udp, Socket, IP, InPortNo, Packet} ->            % The message is an UDP packet. 
            {ok, Pid} = Module:start(),                   % Start the worker to do something with it.
            Pid ! {udp, Socket, IP, InPortNo, Packet},    % Send the packet to the worker.
	    ok = inet:setopts(LSocket, [{active, once}]), % Enable accepting one packet.
            acceptor(udp, Module, LSocket);               % Wait for the next packet.
        _ ->
            acceptor(udp, Module, LSocket)   % TODO: do some logging ? we received an unexpectde message.
    
    end.
    
%% @doc Called by gen_server to initialize the launcher.
%% @private Only gen_server should call this function.
%% @since 0.1.0
init({Module, tcp, Port}) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, {Module, tcp, Port}]),
    process_flag(trap_exit, true),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, once}]), % Open the tcp port.
    spawn_link(?MODULE, acceptor, [tcp, Module, LSocket]),  % Launch the acceptor.
    {ok, {Module, tcp, LSocket}};                           % We are done.
init({Module, udp, Port}) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, {Module, udp, Port}]),
    process_flag(trap_exit, true),
    {ok, LSocket} = gen_udp:open(Port, [{active, once}]),        % Open the udp port.
    Pid = spawn_link(?MODULE, acceptor, [udp, Module, LSocket]), % Run the acceptor
    gen_udp:controlling_process(LSocket, Pid),                   % and let it receive the (udp) messages.
    {ok, {Module, udp, LSocket}}.                                % We are done.

%% @doc No calls to answer.
%% @private Only gen_server should call this function.
%% @since 0.1.0
handle_call(_Request, _From, State) ->
    %%io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

%% @doc The only cast to answer is to stop.
%% @private Only gen_server should call this function.
%% @since 0.1.0
handle_cast(stop, State) ->
    %%io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, stop, State]),
    {stop, normal, State};
handle_cast(_Request, State) ->
    %%io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

%% @doc No other signals to answer.
%% @private Only gen_server should call this function.
%% @since 0.1.0
handle_info(_Info, State) ->
    %%io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.

%% @doc On termination, close the sockets.
%% @private Only gen_server should call this function.
%% @since 0.1.0
terminate(_Reason, {_Module, tcp, LSocket}) ->
    %%io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, {_Module, tcp, LSocket}]),
    gen_tcp:close(LSocket), % Close the socket, we are done.
    ok;
terminate(_Reason, {_Module, udp, Socket}) ->
    %%io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, {_Module, udp, Socket}]),
    gen_udp:close(Socket), % Close the socket, we are done
    ok.

%% @doc Err... &lt;sarcasm&gt;code changes ?&lt;/sarcasm&gt;
%% @private I think no one is interested in this function, yet.
%% @since 0.1.0
code_change(_OldVsn, State, _Extra) ->
    %%io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.
