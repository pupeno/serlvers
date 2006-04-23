%%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%%
%%% This file is part of Serlvers.
%%%
%%% Serlvers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%%% Serlvers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%%% You should have received a copy of the GNU General Public License along with Serlvers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%%% Linking Serlvers statically or dynamically with other modules is making a combined work based on Serlvers. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%%% In addition, as a special exception, the copyright holders of Serlvers give you permission to combine Serlvers program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Serlvers and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%% @doc The gen_dns behaviour is used to implement Dns servers according to <a href="http://www.ietf.org/rfc/rfc1034.txt">RFC1034</a> and <a href="http://www.ietf.org/rfc/rfc1035.txt">RFC1035</a>.
%%% <p>To make a dns server make a module implementing the behaviour gen_dns, that requires the following functions:</p>
%%% <ul>
%%%  <li>...</li>
%%% </ul>
%%% @see launcher.
%%% @since 0.2

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
	  id,        % Assigned by the program that generates queries. This identifier is copied to the corresponding reply and can be used by the requester to match up replies to outstanding queries.
	  qr,        % Whether this message is a query ('False'), or a response ('True').
	  opcode,    % Kind of query in this message.  This value is set by the originator of a query and copied into the response.
	  aa,        % The responding name server is an authority for the domain name in question section.
	  tc,        % Specifies that this message was truncated due to length greater than that permitted on the transmission channel.
	  rd,        % This may be set in a query and is copied into the response.  If RD is set, it directs the name server to pursue the query recursively.
	  ra,        % Is set or cleared in a response, and denotes whether recursive query support is available in the name server.
	  rcode,     % Type of the response.
	  question,  % The question for the name server.
	  answer,    % 'ResourceRecord's answering the question.
	  authority, % 'ResourceRecord's pointing toward an authority.
	  additional % 'ResourceRecord's holding additional information.
	 }).

-record(question, {
	  qname, % A domain name represented as a sequence of strings
	  qtype, % Specifies the type of the query.
	  qclass % Specifies the class of the query. qclass in means Internet.
	  }).

-record(resource_record, {
	  name,  % A domain name to which this resource record pertains.
	  type,  % This field specifies the meaning of the data in the RDATA field.
	  class, % Specify the class of the data in the RDATA field.
	  ttl,   % A 32 bit unsigned integer that specifies the time interval (in seconds) that the resource record may be cached before it should be discarded.  Zero values are interpreted to mean that the RR can only be used for the transaction in progress, and should not be cached.
	  rdata  % Describes the resource.  The format of this information varies according to the TYPE and CLASS of the resource record. For example, the if the TYPE is A and the CLASS is IN, the RDATA field is a 4 octet ARPA Internet address.
	 }).

%% @doc Function used by Erlang (compiler?) to ensure that a module implementing gen_dns really is exporting the needed functions.
%% @private Only Erlang itself should call this function.
%% @since 0.2
behaviour_info(callbacks) ->
    [{init, 1}, %{daytime, 1}, 
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
    Message = parse_message(list_to_binary(Packet)),
    io:fwrite("Message = ~w.~n", [Message]),
    gen_udp:send(Socket, IP, InPortNo, "huhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhu"),
    {stop, normal, {Module, ModState}};
handle_info({tcp, Socket, Data}, {Module, ModState}) -> % Handle TCP queries.
    io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {tcp, Socket, Data} , {Module, ModState}]),
    Message = parse_message(list_to_binary(Data)),
    io:fwrite("Message = ~w.~n", [Message]),
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
    %%io:fwrite("~w:parse_message(~w)~n", [?MODULE, RawMsg]),
    <<ID:16, QR:1, Opcode:4, AA:1, TC:1, RD:1, RA:1, _Z:3, RCODE:4, QDCOUNT:16, 
     ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Body/binary>> = RawMsg,
    {Questions, Rest} = parse_questions(QDCOUNT, Body),
    {Answer, Rest2} = parse_resource_records(ANCOUNT, Rest),
    {Authority, Rest3} = parse_resource_records(NSCOUNT, Rest2),
    {Additional, _Rest4} = parse_resource_records(ARCOUNT, Rest3),
    #dns_message{id = ID, qr = qr_to_atom(QR), opcode = opcode_to_atom(Opcode),
		 aa = bool_to_atom(AA), tc = bool_to_atom(TC), rd = bool_to_atom(RD),
		 ra = bool_to_atom(RA), rcode = rcode_to_atom(RCODE), question = Questions,
		 answer = Answer, authority = Authority, additional = Additional}.

%% @doc Parse the query section of a DNS message.
%% @private Internal helper function.
%% @since 0.2
parse_questions(Count, Body) ->
    %%io:fwrite("~w:parse_questions(~w, ~w)~n", [?MODULE, Count, Body]),
    parse_questions(Count, Body, []).

parse_questions(0, Body, Questions) ->
    %%io:fwrite("~w:parse_questions(~w, ~w, ~w)~n", [?MODULE, 0, Body, Questions]),
    {lists:reverse(Questions), Body};
parse_questions(Count, Body, Questions) ->
    %%io:fwrite("~w:parse_questions(~w, ~w, ~w)~n", [?MODULE, Count, Body, Questions]),
    {QNAME, <<QTYPE:16, QCLASS:16, Rest/binary>>} = parse_label(Body),
    parse_questions(Count - 1, Rest,
		    [#question{qname = QNAME, qtype = qtype_to_atom(QTYPE),
			       qclass = qclass_to_atom(QCLASS)}|
		     Questions]).

%% @doc Parse the resource records.
%% @private Internal helper function.
%% @since 0.2
parse_resource_records(Count, Body) ->
    %%io:fwrite("~w:parse_resource_records(~w, ~w)~n", [?MODULE, Count, Body]),
    parse_resource_records(Count, Body, []).

parse_resource_records(0, Body, RRs) ->
    %%io:fwrite("~w:parse_resource_records(~w, ~w, ~w)~n", [?MODULE, 0, Body, RRs]),
    {lists:reverse(RRs), Body};
parse_resource_records(Count, Body, RRs) ->
    {NAME, <<TYPE:16, CLASS:16, TTL:32, RDLENGTH:16, Rest/binary>>} = parse_label(Body),
    case type_to_atom(TYPE) of
	a ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	ns -> 
	    RDATA = unspecified,
	    Rest2 = Rest;
	md ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	mf ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	cname ->
	    {RDATA, Rest2} = parse_label(Rest);
	soa ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	mb ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	mg ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	mr ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	null ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	wks ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	ptr -> 
	    RDATA = unspecified,
	    Rest2 = Rest;
	hinfo ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	minfo ->
	    RDATA = unspecified,
	    Rest2 = Rest;
	mx ->
	    RDATA = unspecified,
	    Rest2 = Rest
    end,	    
    parse_resource_records(Count - 1, Rest2,
			   [#resource_record{name = NAME, type = type_to_atom(TYPE),
					     class = class_to_atom(CLASS), ttl = TTL,
					     rdata = RDATA}|
			   RRs]).
    
%% @doc Parse a DNS label.
%% @private Internal helper function.
%% @since 0.2
parse_label(Body) ->
    %%io:fwrite("~w:parse_label(~p)~n", [?MODULE, Body]),
    parse_label([], Body).
parse_label(Labels, Body) ->
    %%io:fwrite("~w:parse_label(~p, ~p)~n", [?MODULE, Labels, Body]),
    <<Length:8, Rest/binary>> = Body,
    if Length == 0 ->
	    {lists:reverse(Labels), Rest};
       Length /= 0 ->
	    <<Label:Length/binary-unit:8, Rest2/binary>> = Rest,
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
type_to_atom(15) -> mx.

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
class_to_atom(4) -> hs.

%% @doc Turn a numeric DNS qclass into an atom.
%% @private Internal helper function.
%% @since 0.2
qclass_to_atom(255) -> any;
qclass_to_atom(Class) -> class_to_atom(Class).

%% @doc Turn a numeric DNS QR into an atom.
%% @private Internal helper function.
%% @since 0.2
qr_to_atom(0) -> query_;
qr_to_atom(1) -> response.
		      
%% @doc Turn a numeric DNS Opcode into an atom.
%% @private Internal helper function.
%% @since 0.2
opcode_to_atom(0) -> query_;
opcode_to_atom(1) -> iquery;
opcode_to_atom(2) -> status.

%% @doc Turn a numeric boolean where 0 is false and 1 is true into an atom.
%% @private Internal helper function.
%% @since 0.2
bool_to_atom(0) -> false;
bool_to_atom(1) -> true.

%% @doc Turn a numeric DNS RCODE into an atom.
%% @private Internal helper function.
%% @since 0.2
rcode_to_atom(0) -> no_error;
rcode_to_atom(1) -> format_error;
rcode_to_atom(2) -> server_failure;
rcode_to_atom(3) -> name_error;
rcode_to_atom(4) -> not_implemente;
rcode_to_atom(5) -> refused.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tests() ->
    [{"Label parsing", tests_label_parsing()},
     {"Question parsing", tests_question_parsing()},
     {"Resource record parsing", tests_resource_record_parsing()},
     {"Message parsing", tests_message_parsing()}].

%% -define(LABELS, [{["com"], <<3, "com">>},
%% 		 {["pupeno"], <<6, "pupeno">>},
%% 		 {["software"], <<8, "software">>},
%% 		 {["packages"], <<8, "packages">>}]).
-define(LABELS, [{["com"], <<3>>},
		 {["pupeno"], <<6>>},
		 {["software"], <<8>>},
		 {["mail"], <<4>>}]).

build_domains(Length) ->
    io:fwrite("~w:build_domains(~p)~n", [?MODULE, Length]),
    build_domains(?LABELS, Length).

build_domains(_Labels, 0) ->
    io:fwrite("~w:build_domains(~p, ~p)~n", [?MODULE, _Labels, 0]),
    [];
build_domains(Labels, 1) ->
    io:fwrite("~w:build_domains(~p, ~p)~n", [?MODULE, Labels, 1]),
    Labels;
build_domains(Labels, 2) ->
    io:fwrite("~w:build_domains(~p, ~p)~n", [?MODULE, Labels, 2]),
    Comb = fun(Head) ->
		   one_label_to_many(Head, Labels) end,
    lists:flatten(lists:map(Comb, Labels)).
    
one_label_to_many({Parsed, Raw}, Labels) ->
    io:fwrite("~w:one_with_many(~p, ~p)~n", [?MODULE, {Parsed, Raw}, Labels]),
    Comb = fun({Parsed2, Raw2}) -> 
		   {lists:append(Parsed, Parsed2), <<Raw/binary, Raw2/binary>>} end,
    lists:map(Comb, Labels).



-define(C, ["com"]).
-define(CB, <<3, "com">>).
-define(PC, ["pupeno"|?C]).
-define(PCB, <<6, "pupeno", ?CB/binary>>).
-define(SPC, ["software"|?PC]).
-define(SPCB, <<8, "software", ?PCB/binary>>).

%list_of_domains(N) ->
%    LabelsCount = random:uniform(5).

%% domain(Length) ->
%%     domain(?DOMAINS, Length).
%% domain(Labels, Length) ->
%%     {I1,I2,I3} = erlang:now(),
%%     random:seed(I1,I2,I3),
%%     domain(Labels, Length, [], <<>>).

%% domain(_Labels, 0, Parsed, Raw) ->
%%     {Parsed, <<Raw/binary, 0>>};
%% domain(Labels, Length, Parsed, Raw) ->
%%     {NewParsed, NewRaw} = one_of(Labels),
%%     io:fwrite("NewParsed = ~w, NewRaw = ~w~n", [NewParsed, NewRaw]),
%%     domain(Labels, Length - 1, [NewParsed|Parsed], <<NewRaw/binary, Raw/binary>>).
    

tests_label_parsing() ->
    Data = [{?C, ?CB}, {?PC, ?PCB}, {?SPC, ?SPCB}],
    tests_label_parsing(1000, Data).

tests_label_parsing(0, _Data) ->
    [];
tests_label_parsing(Count, Data) ->
    {Parsed, Raw} = one_of(Data),
    Noise = list_to_binary(noise()),
    [?_assert({Parsed, Noise} == parse_label(<<Raw/binary, 0, Noise/binary>>)) |
     tests_label_parsing(Count-1, Data)].
    

-define(C_ALL_IN, #question{qname = ?C, qtype = all, qclass = in}).
-define(C_ALL_INB, <<?CB/binary, 0, 255:16, 1:16>>).
-define(C_MX_CS, #question{qname = ?C, qtype = mx, qclass = cs}).
-define(C_MX_CSB, <<?CB/binary, 0, 15:16, 2:16>>).
-define(PC_NS_CH, #question{qname = ?PC, qtype = ns, qclass = ch}).
-define(PC_NS_CHB, <<?PCB/binary, 0, 2:16, 3:16>>).
-define(PC_SOA_HS, #question{qname = ?PC, qtype = soa, qclass = hs}).
-define(PC_SOA_HSB, <<?PCB/binary, 0, 6:16, 4:16>>).
-define(SPC_A_ANY, #question{qname = ?SPC, qtype = a, qclass = any}).
-define(SPC_A_ANYB, <<?SPCB/binary, 0, 1:16, 255:16>>).
-define(SPC_PTR_IN, #question{qname = ?SPC, qtype = ptr, qclass = in}).
-define(SPC_PTR_INB, <<?SPCB/binary, 0, 12:16, 1:16>>).


tests_question_parsing() ->
    [{"No question", ?_assert({[], <<>>} == parse_questions(0, <<>>))},
     {"One question",
      [?_assert({[?C_ALL_IN], <<>>} == parse_questions(1, ?C_ALL_INB)),
       ?_assert({[?C_MX_CS], <<>>} == parse_questions(1, ?C_MX_CSB)),
       ?_assert({[?PC_NS_CH], <<>>} == parse_questions(1, ?PC_NS_CHB)),
       ?_assert({[?PC_SOA_HS], <<>>} == parse_questions(1, ?PC_SOA_HSB)),
       ?_assert({[?SPC_A_ANY], <<>>} == parse_questions(1, ?SPC_A_ANYB)),
       ?_assert({[?SPC_PTR_IN], <<>>} == parse_questions(1, ?SPC_PTR_INB))]},
     {"Two questions",
      [?_assert({[?C_ALL_IN, ?C_MX_CS], <<>>} == 
		parse_questions(2, <<?C_ALL_INB/binary, ?C_MX_CSB/binary>>)),
       ?_assert({[?C_MX_CS, ?PC_NS_CH], <<>>} == 
		parse_questions(2, <<?C_MX_CSB/binary, ?PC_NS_CHB/binary>>)),
       ?_assert({[?PC_NS_CH, ?PC_SOA_HS], <<>>} == 
		parse_questions(2, <<?PC_NS_CHB/binary, ?PC_SOA_HSB/binary>>)),
       ?_assert({[?PC_SOA_HS, ?SPC_A_ANY], <<>>} == 
		parse_questions(2, <<?PC_SOA_HSB/binary, ?SPC_A_ANYB/binary>>)),
       ?_assert({[?SPC_A_ANY, ?SPC_PTR_IN], <<>>} == 
		parse_questions(2, <<?SPC_A_ANYB/binary, ?SPC_PTR_INB/binary>>)),
       ?_assert({[?SPC_PTR_IN, ?C_ALL_IN], <<>>} == 
		parse_questions(2, <<?SPC_PTR_INB/binary, ?C_ALL_INB/binary>>))]},
     {"Three questions",
      [?_assert({[?C_ALL_IN, ?C_MX_CS, ?PC_NS_CH], <<>>} == 
		parse_questions(3, <<?C_ALL_INB/binary, ?C_MX_CSB/binary, 
				    ?PC_NS_CHB/binary>>)),
       ?_assert({[?C_MX_CS, ?PC_NS_CH, ?PC_SOA_HS], <<>>} == 
		parse_questions(3, <<?C_MX_CSB/binary, ?PC_NS_CHB/binary, 
				    ?PC_SOA_HSB/binary>>)),
       ?_assert({[?PC_NS_CH, ?PC_SOA_HS, ?SPC_A_ANY], <<>>} == 
		parse_questions(3, <<?PC_NS_CHB/binary, ?PC_SOA_HSB/binary, 
				    ?SPC_A_ANYB/binary>>)),
       ?_assert({[?PC_SOA_HS, ?SPC_A_ANY, ?SPC_PTR_IN], <<>>} == 
		parse_questions(3, <<?PC_SOA_HSB/binary, ?SPC_A_ANYB/binary, 
				    ?SPC_PTR_INB/binary>>)),
       ?_assert({[?SPC_A_ANY, ?SPC_PTR_IN, ?C_ALL_IN], <<>>} == 
		parse_questions(3, <<?SPC_A_ANYB/binary, ?SPC_PTR_INB/binary, 
				    ?C_ALL_INB/binary>>)),
       ?_assert({[?SPC_PTR_IN, ?C_ALL_IN, ?C_MX_CS], <<>>} == 
		parse_questions(3, <<?SPC_PTR_INB/binary, ?C_ALL_INB/binary, 
				    ?C_MX_CSB/binary>>))]},
    {"Six questions",
     ?_assert({[?C_ALL_IN, ?C_MX_CS, ?PC_NS_CH, ?PC_SOA_HS, ?SPC_A_ANY, ?SPC_PTR_IN], <<>>} == 
	      parse_questions(6,
			      <<?C_ALL_INB/binary, ?C_MX_CSB/binary, 
			       ?PC_NS_CHB/binary, ?PC_SOA_HSB/binary,
			       ?SPC_A_ANYB/binary, ?SPC_PTR_INB/binary>>))}].

-define(PCB_, <<?PCB/binary, 0>>).

tests_resource_record_parsing() ->
    PCB_L = length(binary_to_list(?PCB_)),
    [{"No RR", ?_assert({[], <<>>} == parse_resource_records(0, <<>>))},
     {"One RR", 
      [?_assert({[#resource_record{name = ?SPC,
				   type = cname,
				   class = in,
				   ttl = 176800,
				   rdata = ?PC}], <<>>} == 
		parse_resource_records(1, <<?SPCB/binary, 0:8,  5:16, 1:16, 176800:32, 
					   PCB_L:16, ?PCB_/binary>>)),
       ?_assert({[#resource_record{name = ?SPC,
				   type = cname,
				   class = in,
				   ttl = 176800,
				   rdata = ?PC}], <<>>} == 
		parse_resource_records(1, <<?SPCB/binary, 0:8,  5:16, 1:16, 176800:32, 
					   PCB_L:16, ?PCB_/binary>>))]}].

tests_message_parsing() ->
    [?_assert(#dns_message{id = 63296, 
			   qr = query_,
			   opcode = query_,
			   aa = false,
			   tc = false,
			   rd = true,
			   ra = false,
			   rcode = no_error,
			   question = [],
			   answer = [],
			   authority = [],
			   additional = []} ==
	     parse_message(<<63296:16, 0:1, 0:4, 0:1, 0:1, 1:1, 0:1, 0:3, 0:4,
			    0:16, 0:16, 0:16, 0:16>>)),
     ?_assert(#dns_message{id = 13346, 
			   qr = response,
			   opcode = status,
			   aa = true,
			   tc = true,
			   rd = false,
			   ra = true,
			   rcode = server_failure,
			   question = [],
			   answer = [],
			   authority = [],
			   additional = []} ==
	     parse_message(<<13346:16, 1:1, 2:4, 1:1, 1:1, 0:1, 1:1, 0:3, 2:4,
			    0:16, 0:16, 0:16, 0:16>>)),
    ?_assert(#dns_message{id = 63296, 
			   qr = query_,
			   opcode = iquery,
			   aa = false,
			   tc = false,
			   rd = true,
			   ra = false,
			   rcode = refused,
			   question = [?C_ALL_IN],
			   answer = [],
			   authority = [],
			   additional = []} ==
	     parse_message(<<63296:16, 0:1, 1:4, 0:1, 0:1, 1:1, 0:1, 0:3, 5:4,
			    1:16, 0:16, 0:16, 0:16,?C_ALL_INB/binary >>)),
    ?_assert(#dns_message{id = 63296, 
			   qr = query_,
			   opcode = query_,
			   aa = false,
			   tc = false,
			   rd = true,
			   ra = false,
			   rcode = format_error,
			   question = [?C_ALL_IN, ?C_MX_CS, ?PC_NS_CH, 
				       ?PC_SOA_HS, ?SPC_A_ANY, ?SPC_PTR_IN],
			   answer = [],
			   authority = [],
			   additional = []} ==
	     parse_message(<<63296:16, 0:1, 0:4, 0:1, 0:1, 1:1, 0:1, 0:3, 1:4,
			    6:16, 0:16, 0:16, 0:16, ?C_ALL_INB/binary, ?C_MX_CSB/binary, 
			    ?PC_NS_CHB/binary, ?PC_SOA_HSB/binary, ?SPC_A_ANYB/binary,
			    ?SPC_PTR_INB/binary>>))].

test() ->
    eunit:test(tests()).

%% @doc Return one random item out of a list.
%% @private Internal helper function.
%% @since 0.2
one_of(L) ->
    %io:fwrite("~w:one_of(~w)~n", [?MODULE, L]),
    lists:nth(random:uniform(length(L)), L).

%% @doc Generate some noise, that is a list of random length (less than 15) with random data.
%%      The porpuse is to insert data in places where the system should not look at.
%% @private Internal helper function.
%% @since 0.2
noise() ->
    {I1,I2,I3} = erlang:now(),
    random:seed(I1,I2,I3),
    noise(random:uniform(15) - 1).

noise(0) -> [];
noise(Max) ->
    [random:uniform(256) - 1 | 
     noise(Max - 1)].
