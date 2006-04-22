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
%% @doc The gen_dns behaviour is used to implement Dns servers according to <a href="http://www.ietf.org/rfc/rfc1034.txt">RFC1034</a> and <a href="http://www.ietf.org/rfc/rfc1035.txt">RFC1035</a>.
%% <p>To make a dns server make a module implementing the behaviour gen_dns, that requires the following functions:</p>
%% <ul>
%%  <li>...</li>
%% </ul>
%% @see launcher.
%% @since 0.2

-module(gen_dns).
-behaviour(gen_server).
-export([start/3, start/4, start_link/3, start_link/4, stop/1]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

-export([test/0, tests/0]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
%%-include_lib("eunit/include/eunit_test.hrl").

%% @doc Structure defining a DNS message. It is based on what is defined on RFC1035 <http://www.ietf.org/rfc/rfc1035.txt> but it has been re-arranged for easy of use and some fields than are not needed where removed (the counts, which can be calculated out of the length of the lists).
%% @since 0.2
-record(dns_message, {
	  id,        %% Assigned by the program that generates queries.  This identifier is copied to the corresponding reply and can be used by the requester to match up replies to outstanding queries.
	  qr,        %% Whether this message is a query ('False'), or a response ('True').
	  opcode,    %% Kind of query in this message.  This value is set by the originator of a query and copied into the response.
	  aa,        %% The responding name server is an authority for the domain name in question section.
	  tc,        %% Specifies that this message was truncated due to length greater than that permitted on the transmission channel.
	  rd,        %% This may be set in a query and is copied into the response.  If RD is set, it directs the name server to pursue the query recursively.
	  ra,        %% Is set or cleared in a response, and denotes whether recursive query support is available in the name server.
	  rcode,     %% Type of the response.
	  question,  %% The question for the name server.
	  answer,    %% 'ResourceRecord's answering the question.
	  authority, %% 'ResourceRecord's pointing toward an authority.
	  additional %% 'ResourceRecord's holding additional information.
	 }).

-record(question, {
	  qname,
	  qtype,
	  qclass
	  }).


%% @doc Function used by Erlang (compiler?) to ensure that a module implementing gen_dns really is exporting the needed functions.
%% @private Only Erlang itself should call this function.
%% @since 0.2
behaviour_info(callbacks) ->
    [{init, 1}, %%{daytime, 1}, 
     {terminate, 2}];
behaviour_info(_) ->
    undefined.

%% @doc Start an unnamed dns server.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start/4
%% @see start_link/3
%% @since 0.2
%% @spec (Module::atom(), Args::term(), Options) -> Result
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(Module, Args, Options) ->
    io:fwrite("~w:start(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start(?MODULE, {Module, Args}, Options).

%% @doc Start a named dns server.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start/3
%% @see start_link/4
%% @since 0.2
%% @spec (SupName, Module::atom(), Args::term(), Options) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(SupName, Module, Args, Options) ->
    io:fwrite("~w:start(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start(SupName, ?MODULE, {Module, Args}, Options).

%% @doc Start an unnamed dns server and link to it.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start_link/4
%% @see start/3
%% @since 0.2
%% @spec (Module::atom(), Args::term(), Options) -> Result
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(Module, Args, Options) ->
    io:fwrite("~w:start_link(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start_link(?MODULE, {Module, Args}, Options).

%% @doc Start a named dns server and link to it.
%% <p>The parameters are exactly the same as gen_server and most of them (like Options) are passed as is to gen_server.</p>
%% @see start_link/3
%% @see start/4
%% @since 0.2
%% @spec (SupName, Module::atom(), Args::term(), Options) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(SupName, Module, Args, Options) ->
    io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, Options).

%% @doc Stop a named process.
%% @see start/3
%% @see start/4
%% @see start_link/3
%% @see start_link/4
%% @since 0.2
%% @spec (Name) -> ok
%%   Name = atom() | {local, atom()} | {global, atom()}
stop(Process) ->
    io:fwrite("~w:stop(~w)~n", [?MODULE, Process]),
    gen_server:cast(Process, stop).

%% @doc This function gets called by gen_server to initialize the module. After some basic internal initialization the init function of the module implementing the particular dns server gets called (same as this module implementing a particular gen_server).
%% @private Only gen_server should call this function.
%% @since 0.2
init({Module, Args}) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, {Module, Args}]),
    process_flag(trap_exit, true),
    {ok, ModState} = Module:init(Args),
    {ok, {Module, ModState}}.

%% @doc The base module, gen_server may call this function. Currently there's nothing to be done here.
%% @private Only gen_server should call this function.
%% @since 0.2
handle_call(_Request, _From, State) ->
    io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

%% @doc This function handles stoping the dns server.
%% @private Only gen_server should call this function.
%% @since 0.2
handle_cast(stop, State) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, stop, State]),
    {stop, normal, State};
handle_cast(_Request, State) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

%% @doc This function handles the udp and tcp case of dns.
%% @private Only gen_server should call this function.
%% @since 0.2
handle_info({connected, Socket}, {Module, ModState}) ->
    io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, {connected, Socket}, {Module, ModState}]),
%    %%{Reply, NewModState} = Module:dns(ModState),
%    %%gen_tcp:send(Socket, Reply),
%    {stop, normal, {Module, NewModState}};
    {noreply, {Module, ModState}};
handle_info({udp, Socket, IP, InPortNo, Packet}, {Module, ModState}) -> % Handle UDP packages.
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {udp, Socket, IP, InPortNo, Packet} , {Module, ModState}]),
    %%{Reply, NewModState} = Module:dns(ModState), % Generate the reply.
    %%gen_udp:send(Socket, IP, InPortNo, Reply),       % Send the reply.
    %%ok = inet:setopts(Socket, [{active, once}]),     % Enable receiving of packages, get the next one.
    Query = parse_message(list_to_binary(Packet)),
    io:fwrite("Query = ~w.~n", [Query]),
    gen_udp:send(Socket, IP, InPortNo, "huhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhu"),
    {stop, normal, {Module, ModState}};
handle_info({tcp, Socket, Data}, {Module, ModState}) -> % Handle TCP queries.
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {tcp, Socket, Data} , {Module, ModState}]),
    Query = parse_message(list_to_binary(Data)),
    io:fwrite("Query = ~w.~n", [Query]),
    gen_tcp:send(Socket, "caca"),
    {stop, normal, {Module, ModState}};
handle_info(_Info, State) ->
    io:fwrite("~w:handle_info(~w, ~w)~nUnknow message.~n", [?MODULE, _Info, State]),
    {noreply, normal, State}.

%% @doc This function get's called by the underling gen_server and we just pass it over to the module implementing a dns server.
%% @private Only gen_server should call this function.
%% @since 0.2
terminate(Reason, {Module, ModState}) ->
    io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, Reason, {Module, ModState}]),
    ok = Module:terminate(Reason, ModState),
    ok.

%% @doc Err... &lt;sarcasm&gt;code changes ?&lt;/sarcasm&gt;
%% @private I think no one is interested in this function, yet.
%% @since 0.2
code_change(_OldVsn, State, _Extra) ->
    io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.

%% @doc Given a binary string representing a DNS message (the incomming from the network) return the same DNS message represented as records.
%% @private Internal helper function.
%% @since 0.2
parse_message(RawMsg) ->
    io:fwrite("~w:parse_message(~w)~n", [?MODULE, RawMsg]),
    <<ID:16, QR:1, Opcode:4, AA:1, TC:1, RD:1, RA:1, _Z:3, RCODE:4,
     QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Body/binary>> = RawMsg,
    Msg = #dns_message{id = ID, qr = QR, opcode = Opcode, aa = AA, tc = TC, rd = RD, ra = RA, rcode = RCODE},
    io:fwrite("QDCOUNT = ~w, ANCOUNT = ~w, NSCOUNT = ~w, ARCOUNT = ~w, Body = ~w~n", [QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT, Body]),
    io:fwrite("DNS Message = ~w.~n", [Msg]),
    Questions = parse_questions(QDCOUNT, Body),
    io:fwrite("Questions = ~p~n", [Questions]),
    Msg.

%% @doc Parse the query section of a DNS message.
%% @private Internal helper function.
%% @since 0.2
parse_questions(0, _Body) ->
    io:fwrite("~w:parse_questions(~w, ~w)~n", [?MODULE, 0, _Body]),
    [];
parse_questions(Count, Body) ->
    io:fwrite("~w:parse_questions(~w, ~w)~n", [?MODULE, Count, Body]),
    {QNAME, <<QTYPE:16, QCLASS:16, Rest/binary>>} = parse_label(Body),
    io:fwrite("QNAME = ~p, QTYPE = ~p, QCLASS = ~p~n", [QNAME, QTYPE, QCLASS]),
    [#question{qname = QNAME, qtype = QTYPE, qclass = QCLASS}, parse_questions(Count - 1, Rest)].

%% @doc Parse a DNS label.
%% @private Internal helper function.
%% @since 0.2
parse_label(Body) ->
    %%io:fwrite("~w:parse_label(~p)~n", [?MODULE, Body]),
    parse_label([], Body).
parse_label(Labels, Body) ->
    %%io:fwrite("~w:parse_label(~p, ~p)~n", [?MODULE, Labels, Body]),
    <<Length:8, Rest/binary>> = Body,
    %%io:fwrite("Length = ~w", [Length]), 
    if
	Length == 0 ->
	    %%io:fwrite("~n"),
	    {lists:reverse(Labels), Rest};
	Length /= 0 ->
	    <<Label:Length/binary-unit:8, Rest2/binary>> = Rest,
	    %%io:fwrite(", Label = ~p, Rest2 = ~p~n", [binary_to_list(Label), Rest2]),
	    parse_label([binary_to_list(Label)|Labels], Rest2)
    end.

%% @doc Turn a numeric DNS type into an atom.
%% @private Internal helper function.
%% @since 0.2
type_to_atom(1) -> a;
type_to_atom(2) -> ns;
type_to_atom(3) -> md;
type_to_atom(4) -> mf;
type_to_atom(5) -> cname;
type_to_atom(6) -> soa;
type_to_atom(7) -> mb;
type_to_atom(8) -> mg;
type_to_atom(9) -> mr;
type_to_atom(10) -> null;
type_to_atom(11) -> wks;
type_to_atom(12) -> ptr;
type_to_atom(13) -> hinfo;
type_to_atom(14) -> minfo;
type_to_atom(15) -> mx;
type_to_atom(_) -> unknown.

%% @doc Turn a numeric DNS qtype into an atom.
%% @private Internal helper function.
%% @since 0.2
qtype_to_atom(252) -> axfr;
qtype_to_atom(253) -> mailb;
qtype_to_atom(254) -> maila;
qtype_to_atom(255) -> all;
qtype_to_atom(Type) -> type_to_atom(Type).

%% @doc Turn a numeric DNS class into an atom.
%% @private Internal helper function.
%% @since 0.2
class_to_atom(1) -> in;
class_to_atom(2) -> cs;
class_to_atom(3) -> ch;
class_to_atom(4) -> hs;
class_to_atom(_) -> unknown.

%% @doc Turn a numeric DNS qclass into an atom.
%% @private Internal helper function.
%% @since 0.2
qclass_to_atom(255) -> any;
qclass_to_atom(Class) -> class_to_atom(Class).

    
%%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%
tests() ->
    {"Label parsing", tests_label_parsing()}.

tests_label_parsing() ->
    [{"com", ?_assert({["com"], <<>>} == parse_label(<<3, "com", 0>>))},
     {"pupeno.com", ?_assert({["pupeno", "com"], <<>>} == parse_label(<<6, "pupeno", 3, "com", 0>>))},
     {"software.pupeno.com", ?_assert({["software", "pupeno", "com"], <<>>} == parse_label(<<8, "software", 6, "pupeno", 3, "com", 0>>))},
     {"com + extra", ?_assert({["com"], <<"trailing trash">>} == parse_label(<<3, "com", 0, "trailing trash">>))},
     {"pupeno.com + extra", ?_assert({["pupeno", "com"], <<"whatever">>} == parse_label(<<6, "pupeno", 3, "com", 0, "whatever">>))},
     {"software.pupeno.com + extra", ?_assert({["software", "pupeno", "com"], <<"who cares ?">>} == parse_label(<<8, "software", 6, "pupeno", 3, "com", 0, "who cares ?">>))}].

%% tests_question_parsing() ->
%%     [{"com

test() ->
    eunit:test(tests()).
