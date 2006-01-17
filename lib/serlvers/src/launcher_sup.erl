%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of ErServers.
%%
%% ErServers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% ErServers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with ErServers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking ErServers statically or dynamically with other modules is making a combined work based on ErServers. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of ErServers give you permission to combine ErServers program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for ErServers and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

-module(launcher_sup).
-behaviour(supervisor).
-export([start_link/4, stop/0, children/0]).
-export([init/1]).

start_link(EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort) ->
    %io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, {EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort}).

stop() ->
    case (whereis(echo_sup)) of
        undefined ->
            {ok, echo_sup_not_running};
        Pid ->
            {ok, exit(Pid, normal)}
    end.

children() ->
    %io:fwrite("~w:which_children()~n", [?MODULE]),
    supervisor:which_children(?MODULE).

init({EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort}) ->
    %io:fwrite("~w:init(~w)~n", [?MODULE, {EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort}]),
    {ok, {{one_for_one, 1, 5},
          [{echo_udp,
            {launcher, start_link, [{local, echo_udp_launcher},
                                    echo,
                                    udp,
                                    EchoUDPPort]},
            permanent, 1000, worker, [launcher]},
           {echo_tcp,
            {launcher, start_link, [{local, echo_tcp_launcher},
                                    echo,
                                    tcp,
                                    EchoTCPPort]},
            permanent, 1000, worker, [launcher]},
           {daytime_udp,
            {launcher, start_link, [{local, daytime_udp_launcher},
                                    daytime,
                                    udp,
                                    DaytimeUDPPort]},
            permanent, 1000, worker, [launcher]},
           {daytime_tcp,
            {launcher, start_link, [{local, daytime_tcp_launcher},
                                    daytime,
                                    tcp,
                                    DaytimeTCPPort]},
            permanent, 1000, worker, [launcher]}]}}.
