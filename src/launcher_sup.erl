-module(launcher_sup).
-behaviour(supervisor).
-export([start_link/4, stop/0, which_children/0]).
-export([init/1]).

start_link(EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort) ->
    io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, {EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort}).

stop() ->
    case (whereis(echo_sup)) of
        undefined ->
            {ok, echo_sup_not_running};
        Pid ->
            {ok, exit(Pid, normal)}
    end.

which_children() ->
    io:fwrite("~w:which_children()~n", [?MODULE]),
    supervisor:which_children(?MODULE).

init({EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {EchoUDPPort, EchoTCPPort, DaytimeUDPPort, DaytimeTCPPort}]),
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
