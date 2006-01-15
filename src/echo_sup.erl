-module(echo_sup).
-behaviour(supervisor).
-export([start_link/2]).
-export([init/1]).

start_link(UDPPort, TCPPort) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {UDPPort, TCPPort}).

init({UDPPort, TCPPort}) ->
    {ok, {{one_for_one, 1, 60},
          [{echo_tcp, {echo_tcp, start_link, [TCPPort]}, permanent, 1000, worker, [echo_tcp]},
	   {echo_udp, {echo_udp, start_link, [UDPPort]}, permanent, 1000, worker, [echo_udp]}]}}.
