%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of ErServers.
%%
%% ErServers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% ErServers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with ErServers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking ErServers statically or dynamically with other modules is making a combined work based on ErServers. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of ErServers give you permission to combine ErServers program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for ErServers and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%% @doc The gen_daytime behaviour is used to implement Daytime servers according to <a href="http://www.ietf.org/rfc/rfc867.txt">RFC867</a>.
%% @see launcher.

-module(gen_daytime).
-behaviour(gen_server).
-export([start/3, start/4, start_link/3, start_link/4]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1}, {daytime, 1}, {terminate, 2}];
behaviour_info(_) ->
    undefined.

%% API
start(Module, Args, Options) ->
    %io:fwrite("~w:start(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start(?MODULE, {Module, Args}, Options).

start(SupName, Module, Args, Options) ->
    %io:fwrite("~w:start(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start(SupName, ?MODULE, {Module, Args}, Options).

start_link(Module, Args, Options) ->
    %io:fwrite("~w:start_link(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start_link(?MODULE, {Module, Args}, Options).

start_link(SupName, Module, Args, Options) ->
    %io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, Options).

%% Callbacks.
init({Module, Args}) ->
    %io:fwrite("~w:init(~w)~n", [?MODULE, {Module, Args}]),
    process_flag(trap_exit, true),
    {ok, ModState} = Module:init(Args),
    {ok, {Module, ModState}}.

handle_call(_Request, _From, State) ->
    %io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

handle_cast(stop, State) ->
    %io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, stop, State]),
    {stop, normal, State};
handle_cast({connected, Socket}, {Module, ModState}) ->
    %io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, {started, Socket}, {Module, ModState}]),
    {Reply, NewModState} = Module:daytime(ModState),
    gen_tcp:send(Socket, Reply),
    {stop, normal, {Module, NewModState}};
handle_cast(_Request, State) ->
    %io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

handle_info({udp, Socket, IP, InPortNo, _Packet}, {Module, ModState}) -> % Handle UDP packages.
    %io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {udp, Socket, IP, InPortNo, _Packet} , {Module, ModState}]),
    {Reply, NewModState} = Module:daytime(ModState), % Generate the reply.
    gen_udp:send(Socket, IP, InPortNo, Reply),       % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]),     % Enable receiving of packages, get the next one.
    {noreply, {Module, NewModState}};
handle_info(_Info, State) ->
    %io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.
    
terminate(Reason, {Module, ModState}) ->
    %io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, Reason, {Module, ModState}]),
    ok = Module:terminate(Reason, ModState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.
    