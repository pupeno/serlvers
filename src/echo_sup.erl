-module(echo_sup).
-behaviour(supervisor).
-export([start_link/2, stop/0, which_children/0]).
-export([init/1]).

start_link(UDPPort, TCPPort) ->
    io:fwrite("~w:start_link(~w, ~w)~n", [?MODULE, UDPPort, TCPPort]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, {UDPPort, TCPPort}).

stop() ->
    exit(?MODULE, normal).

which_children() ->
    io:fwrite("~w:which_children()~n", [?MODULE]),
    supervisor:which_children(?MODULE).

init({UDPPort, TCPPort}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {UDPPort, TCPPort}]),
    {ok, {{one_for_one, 1, 5},
          [{echo_tcp, 
	    {echo_launcher, start_link, [{local, echo_tcp_launcher}, {tcp, TCPPort}]},
	    permanent, 1000, worker, [echo_launcher]},
	   {echo_udp, 
	    {echo_launcher, start_link, [{local, echo_udp_launcher}, {udp, UDPPort}]},
	    permanent, 1000, worker, [echo_launcher]}]}}.
