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
%% @doc TODO: write documentation.
%% @see gen_daytime.
%% @see gen_echo.

-module(launcher).
-behaviour(gen_server).
-export([start_link/3, start_link/4]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([acceptor/2]).

start_link(Protocol, Transport, Port) ->
    %io:fwrite("~w:start_link(~w, ~w, ~w)~n", [?MODULE, Protocol, Transport, Port]),
    gen_server:start_link(?MODULE, {Protocol, Transport, Port}, []).

start_link(SupName, Protocol, Transport, Port) ->
    %io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Protocol, Transport, Port]),
    gen_server:start_link(SupName, ?MODULE, {Protocol, Transport, Port}, []).

%% The follwing function is in charge of accept new TCP connections.
acceptor(Protocol, LSocket) ->
    %io:fwrite("~w:acceptor(~w)~n", [?MODULE, LSocket]),
    case gen_tcp:accept(LSocket) of                                % Wait for an incomming connection.
        {ok, Socket} ->                                            % Got a succesfull incomming connection.
            case Protocol:start() of                               % Try to run a worker.
                {ok, Pid} ->                                       % Worker running.
                    ok = gen_tcp:controlling_process(Socket, Pid), % Let the worker control this connection.
                    gen_server:cast(Pid, {connected, Socket}),     % Worker, wake up, you have to work (maybe).
                    acceptor(Protocol, LSocket);                   % Show must go on.
                {error, Error} ->                                  % Worker could not be run.
                    {stop, {Protocol, LSocket, Error}}             % Try to return, hopefully, enough information to found out what the error was.
            end;
	{error, Reason} ->                                         % Error accepting connection.
	    {stop, {Protocol, LSocket, Reason}}                    % Try to return, hopefully, enough information to found out what the error was.
    end.

%% Callbacks.
init({Protocol, tcp, Port}) ->
    %io:fwrite("~w:init(~w)~n", [?MODULE, {Protocol, tcp, Port}]),
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [{active, once}]) of              % Try to open the port.
	{ok, LSocket} ->                                        % Port opened.
	    spawn_link(?MODULE, acceptor, [Protocol, LSocket]), % Launch the acceptor.
	    {ok, {Protocol, tcp, LSocket}};                     % We are done.
	{error, Reason} ->                                      % Error opening port.
	    {stop, {Protocol, tcp, Port, Reason}}               % Try to return, hopefully, enough information to found out what the error was.
    end;
init({Protocol, udp, Port}) ->
    %io:fwrite("~w:init(~w)~n", [?MODULE, {Protocol, udp, Port}]),
    process_flag(trap_exit, true),
    case gen_udp:open(Port, [{active, once}]) of                          % Try to open the udp port.
        {ok, Socket} ->                                                   % Port opened.
            case Protocol:start_link({local, udpWorkerName(Protocol)}) of % Try to run a worker for this UDP port.
                {ok, Pid} ->                                              % Worker running.
                    gen_udp:controlling_process(Socket, Pid),             % Give the UDP port to the worker.
                    {ok, {Protocol, udp, Socket}};                        % Done.
                {error, Error} ->                                         % Worker could not be run.
                    {stop, {Protocol, udp, Socket, Error}}                % Try to return, hopefully, enough information to found out what the error was.
            end;
         	{error, Reason} ->                                        % Error opening port.
	    {stop, {Protocol, tcp, Port, Reason}}                         % Try to return, hopefully, enough information to found out what the error was.
    end.                
  
handle_call(_Request, _From, State) ->
    %io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

handle_cast(_Request, State) ->
    %io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

handle_info(_Info, State) ->
    %io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.
    
terminate(_Reason, {_Protocol, tcp, LSocket}) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, {_Protocol, tcp, LSocket}]),
    gen_tcp:close(LSocket), % Close the socket, we are done.
    ok;
terminate(_Reason, {_Protocol, udp, Socket}) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, {_Protocol, udp, Socket}]),
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.

%% Utils.
udpWorkerName(Protocol) ->
    list_to_atom(string:concat(atom_to_list(Protocol), "_udp_worker")).
