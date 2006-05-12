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
%%% @doc Tools to parse and unparse dns messages and their parts.
%%% @see launcher.
%%% @since 0.2

-module(dns_msg).

%%-export([test/1, tests/1]).

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


%% @doc Given a binary string representing a DNS message (the incomming from the network) return the same DNS message represented as records.
%% @private Internal helper function.
%% @since 0.2
parse_message(RawMsg) ->
  %%io:fwrite("~w:parse_message(~w)~n", [?MODULE, RawMsg]),
  <<ID:16, QR:1, Opcode:4, AA:1, TC:1, RD:1, RA:1, _Z:3, RCODE:4, QDCOUNT:16, 
   ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Body/binary>> = RawMsg,
  {questions, Questions, Rest} = parse_questions(QDCOUNT, Body),
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
  {questions, lists:reverse(Questions), Body};
parse_questions(Count, Body, Questions) ->
  %%io:fwrite("~w:parse_questions(~w, ~w, ~w)~n", [?MODULE, Count, Body, Questions]),
  case parse_domain(Body) of
    {domain, QNAME, <<QTYPE:16, QCLASS:16, Rest/binary>>} ->
      parse_questions(Count - 1, Rest,
		      [#question{qname = QNAME, qtype = qtype_to_atom(QTYPE),
				 qclass = qclass_to_atom(QCLASS)}|
		       Questions]);
    {error, invalid} ->
      {error, invalid}
  end.

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
  %%io:fwrite("~w:parse_resource_records(~w, ~w, ~w)~n", [?MODULE, Count, Body, RRs]),
  {domain, NAME, <<TYPE:16, CLASS:16, TTL:32, _RDLENGTH:16, Rest/binary>>} = parse_domain(Body),
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
      {domain, RDATA, Rest2} = parse_domain(Rest);
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
parse_domain(Body) ->
  parse_domain([], Body).
parse_domain(Labels, <<Length:8, Rest/binary>>) when Length > 0 ->
  case Rest of
    <<Label:Length/binary-unit:8, Rest2/binary>> ->
      parse_domain([binary_to_list(Label)|Labels], Rest2);
    _Other ->
      {error, invalid}
  end;
parse_domain(Labels, <<Length:8, Rest/binary>>) when Length == 0 ->
  {domain, lists:reverse(Labels), Rest};
parse_domain(_Labels, _Body) ->
  {error, invalid}.

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
type_to_atom(16) -> txt.

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
rcode_to_atom(4) -> not_implemented;
rcode_to_atom(5) -> refused.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Testing data

%% Some labels (atoms of domain names) to test the parser.
-define(LABELS, [{correct, ["com"], <<3, "com">>},
 		 {correct, ["s"], <<1, "s">>},
		 {correct,["abcdefghijklmnopqrstuvwxyz-0123456789-abcdefghijklmnopqrstuvwxy"], <<63, "abcdefghijklmnopqrstuvwxyz-0123456789-abcdefghijklmnopqrstuvwxy">>},
		 {error, ["mail"], <<5, "mail">>}]).

%% DNS Types to test the parser.
-define(TYPES, [{correct, a,     << 1:16>>},
		{correct, ns,    << 2:16>>},
		{correct, md,    << 3:16>>},
		{correct, mf,    << 4:16>>},
		{correct, cname, << 5:16>>},
		{correct, soa,   << 6:16>>},
		{correct, mb,    << 7:16>>},
		{correct, mg,    << 8:16>>},
		{correct, mr,    << 9:16>>},
		{correct, null,  <<10:16>>},
		{correct, wks,   <<11:16>>},
		{correct, ptr,   <<12:16>>},
		{correct, hinfo, <<13:16>>},
		{correct, minfo, <<14:16>>},
		{correct, mx,    <<15:16>>},
 		{correct, txt,   <<16:16>>}]).

%% DNS QTypes to test the parser.
-define(QTYPES, [{correct, axfr,  <<252:16>>},
		 {correct, mailb, <<253:16>>},
		 {correct, maila, <<254:16>>},
 		 {correct, all,   <<255:16>>}] ++ ?TYPES).

%% DNS Classes to test the parser.
-define(CLASSES, [{correct, in, <<1:16>>},
		  {correct, cs, <<2:16>>},
		  {correct, ch, <<3:16>>},
 		  {correct, hs, <<4:16>>}]).

%% DNS QClasses to test the parser.
-define(QCLASSES, [{correct, any, <<255:16>>}] ++ ?CLASSES).

%% DNS QRs to test the parser.
-define(QRS, [{correct, query_,   <<0:1>>},
	      {correct, response, <<1:1>>}]).

%% DNS Opcodes to test the parser.
-define(OPCODES, [{correct, query_, <<0:4>>},
		  {correct, iquery, <<1:4>>},
		  {correct, status, <<2:4>>}]).

%% DNS Booleans to test the parser.
-define(BOOLEANS, [{correct, false, <<0:1>>},
		   {correct, true,  <<1:1>>}]).

%% DNS RCodes to test the parser.
-define(RCODES, [{correct, no_error,        <<0:4>>},
		 {correct, format_error,    <<1:4>>},
		 {correct, server_failure,  <<2:4>>},
		 {correct, name_error,      <<3:4>>},
		 {correct, not_implemented, <<4:4>>},
		 {correct, refused,         <<5:4>>}]).

%% @doc Generates and run all tests.
%% @since 0.2
test(Factor, Sample) ->
  Labels = n_of(Sample, ?LABELS),                        %% Take a sample of the labels.
  Domains = n_of(Sample, build_domains(Labels, Factor)), %% Build the domains and take a sample of it.
  DomainParsingTests = domain_parsing_tests(Domains),

  QTypes = n_of(Sample, ?QTYPES),
  QClasses = n_of(Sample, ?QCLASSES),
  Questions = n_of(Sample, build_questions(Domains, QTypes, QClasses, Factor)),
  QuestionsParsingTests = questions_parsing_tests(Questions),

  %% TODO: make these tests dynamic as the previous ones.
  %%Types = n_of(Sample, ?TYPES),
  %%Classes = n_of(Sample, ?CLASSES),
  %%RRs = n_of(Sample, build_resource_records(Domains, Types, Classes, Factor)),
  RRsParsingTests = tests_resource_record_parsing(),

  %% TODO: make these tests dynamic as the previous ones.
  MessageParsingTests = tests_message_parsing(),


  eunit:test(DomainParsingTests ++ 
	     QuestionsParsingTests ++ 
	     RRsParsingTests ++ 
	     MessageParsingTests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Domain Parsing testing %%%%%%%%%%%%%%

%% @doc Using lables Labels, build all possible domains from those of length 1 to length Length. 
%% @private Internal helper function.
%% @since 0.2
build_domains(Labels, Length) ->
  BuildDomains = fun(N, Domains) ->                              % Function to build domains of N length and append it to Domains.
		     Domains ++ build_domains_(Labels, N)
		 end,
  Domains = lists:foldl(BuildDomains, [], lists:seq(1, Length)), % Build all possible domains up to Length.
  NullTerm = fun({Type, ParsedDomain, RawDomain}) ->             % Function to add the null character to the domain.
		 {Type, ParsedDomain, <<RawDomain/binary, 0>>}
	     end,
  lists:map(NullTerm, Domains).                                  % Add the null character to the end of each domains.

%% @doc Having a set of labels build domains names of N labels.
%% @private Internal helper function.
%% @todo Find a better name for this function.
%% @since 0.2
build_domains_(_Labels, 0) -> [];
build_domains_(Labels, 1) -> Labels;
build_domains_(Labels, Length) ->
  Domains = build_domains_(Labels, Length - 1),
  Comb = fun(Label) ->                        % Function to combine one label to NewDomains.
	     one_domain_per_domain(Label, Domains) end, 
  lists:flatten(lists:map(Comb, Labels)).

%% @doc Having one label combine it with each domain of a list.
%% @private Internal helper function.
%% @since 0.2
one_domain_per_domain({Type, Parsed, Raw}, Domains) ->
  Comb = fun({Type2, Parsed2, Raw2}) ->  % Function to combine two domains.
	     if (Type == correct) and (Type2 == correct) ->
		 {correct, lists:append(Parsed, Parsed2), <<Raw/binary, Raw2/binary>>};
		true ->
		 {error, lists:append(Parsed, Parsed2), <<Raw/binary, Raw2/binary>>}
	     end
	 end,
  lists:map(Comb, Domains).

%% @doc Having a list of Domains build all the tests to be used by EUnit.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2
domain_parsing_tests([]) -> [];
domain_parsing_tests([{Type, Parsed, Raw}|Domains]) ->
  Noise = list_to_binary(noise()),
  CRaw = <<Raw/binary, Noise/binary>>,      % Add noise
  CRightParsed = {domain, Parsed, Noise},      % What would be returned if parsing succeds.
  ParsedToTest = (catch parse_domain(CRaw)),   % Perform the parsing.
  Desc = lists:flatten(io_lib:format("~p, ~p", % Some useful description
				     [Type, CRightParsed])),
  case Type of   % What kind of test is it ?
    correct ->
      [{Desc, ?_assert(ParsedToTest == CRightParsed)} |
       domain_parsing_tests(Domains)];
    error   -> 
      [{Desc, ?_assert((ParsedToTest == {error, invalid}) or % We should get an error
		       (ParsedToTest /= CRightParsed))} |    % or plain wrong data (not an exception).
       domain_parsing_tests(Domains)]
  end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Question Parsing testing %%%%%%%%%%%%%

%% @doc Using domains Domains, QTypes and QClasses build all possible questions up to length Length (that is, chained questions). 
%% @private Internal helper function.
%% @since 0.2
build_questions(Domains, QTypes, QClasses, Length) ->
  Questions = build_questions(Domains, QTypes, QClasses),
  BuildQuestions = fun(N, NewQuestions) ->                           % Function to build questions of N length and append it to NewQuestions.
		       NewQuestions ++ build_questions(Questions, N)
		   end,
  lists:foldl(BuildQuestions, [], lists:seq(1, Length)).

%% @doc Make all the possible combinations for a set of Domains, QTypes and QClasses. 
%% @private Internal helper function.
%% @since 0.2
build_questions([], _QTypes, _QClasses) -> [];
build_questions(_Domains, [], _QClasses) -> [];
build_questions(_Domains, _QTypes, []) -> [];
build_questions({DType, DParsed, DRaw}, {QTType, QTParsed, QTRaw}, {QCType, QCParsed, QCRaw}) ->
  Type = if (DType == correct) and (QTType == correct) and (QCType == correct)-> correct;
	    true -> error
	 end,
  {Type, 1, [#question{qname = DParsed, qtype = QTParsed, qclass = QCParsed}],
   <<DRaw/binary, QTRaw/binary, QCRaw/binary>>};
build_questions({DType, DParsed, DRaw}, {QTType, QTParsed, QTRaw}, QClasses) ->
  BuildQuestion = fun(QClass) ->
		      build_questions({DType, DParsed, DRaw},
				      {QTType, QTParsed, QTRaw}, QClass)
		  end,
  lists:flatten(lists:map(BuildQuestion, QClasses));
build_questions({DType, DParsed, DRaw}, QTypes, QClasses) ->
  BuildQuestion = fun(QType) ->
		      build_questions({DType, DParsed, DRaw}, QType, QClasses)
		  end,
  lists:flatten(lists:map(BuildQuestion, QTypes));
build_questions(Domains, QTypes, QClasses) ->
  BuildQuestion = fun(Domain) ->
		      build_questions(Domain, QTypes, QClasses)
		  end,
  lists:flatten(lists:map(BuildQuestion, Domains)).

%% @doc Combine each question in Question with every other item in Questions up to N. 
%% @private Internal helper function.
%% @since 0.2
build_questions(_Questions, 0) -> [];
build_questions(Questions, 1) -> Questions;
build_questions(Questions, N) ->
  NewQuestions = build_questions(Questions, N - 1),
  Comb = fun(Question) ->              % Function to combine one question to NewQuestions.
	     one_question_per_questions(Question, NewQuestions) end, 
  lists:flatten(lists:map(Comb, Questions)).

%% @doc Combine one question with every other list of questions (in Questions).
%% @private Internal helper function.
%% @since 0.2
one_question_per_questions({Type, Count, Parsed, Raw}, Questions) ->
  Comb = fun({Type2, Count2, Parsed2, Raw2}) ->
	     NewType = if (Type == correct) and (Type2 == correct) -> correct;
			  true -> error end,
	     {NewType,
	      Count + Count2,
	      lists:append(Parsed, Parsed2),
	      <<Raw/binary, Raw2/binary>>}
	 end,
  lists:map(Comb, Questions).



%% @doc Having a list of Domains build all the tests to be used by EUnit.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2
questions_parsing_tests([]) -> [];
questions_parsing_tests([{Type, Count, Parsed, Raw}|Questions]) ->
  Noise = list_to_binary(noise()),
  CRaw = <<Raw/binary, Noise/binary>>,                 % Add noise
  CRightParsed = {questions, Parsed, Noise},           % What would be returned if parsing succeds.
  ParsedToTest = (catch parse_questions(Count, CRaw)), % Perform the parsing.
  Desc = lists:flatten(io_lib:format("~p, ~p, ~p, ~p", % Some useful description
				     [Type, CRightParsed, CRaw, ParsedToTest])),
  case Type of   % What kind of test is it ?
    correct ->
      [{Desc, ?_assert(ParsedToTest == CRightParsed)} |
       questions_parsing_tests(Questions)];
    error   -> 
      [{Desc, ?_assert((ParsedToTest == {error, invalid}) or % We should get an error
		       (ParsedToTest /= CRightParsed))} |    % or plain wrong data (not an exception).
       questions_parsing_tests(Questions)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Resource Record Parsing testing %%%%%%%%%

%% @doc . 
%% @private Internal helper function.
%% @since 0.2
build_resource_records(_Domains, _Types, _Classes, _Length) ->
  %%  RDATA = [{cname, Domains},
  %% 	   {hinfo, []},    % Where do we get random hinfos ?
  %% 	   {mx, []},       % Build MX using Domains and a random 16-bits integer.
  %% 	   {ns, Domains},
  %% 	   {ptr, Domains},
  %% 	   {soa, []},      % Build SOA using Domains, root+domains for emails and random values.
  %% 	   {txt, []},      % Build txt with random text.
  %% 	   {a, []},        % Where do we get random IPs ?
  %% 	   {wks, []}],     % is this used anyway ?
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DNS Message Parsing testing %%%%%%%%%%%

%% @doc . 
%% @private Internal helper function.
%% @since 0.2
build_messages(_Questions, _RRs) ->
  [].


%%%%%%%%%%%%%%%%%% Old boring tests %%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(C, ["com"]).
-define(CB, <<3, "com">>).
-define(PC, ["pupeno"|?C]).
-define(PCB, <<6, "pupeno", ?CB/binary>>).
-define(SPC, ["software"|?PC]).
-define(SPCB, <<8, "software", ?PCB/binary>>).
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

-define(PCB_, <<?PCB/binary, 0:8>>).
-define(SPCB_, <<?SPCB/binary, 0:8>>).
tests_resource_record_parsing() ->
  PCB_L = length(binary_to_list(?PCB_)),
  [{"No RR", ?_assert({[], <<>>} == parse_resource_records(0, <<>>))},
   {"One RR", 
    [?_assert({[#resource_record{name = ?SPC,
				 type = cname,
				 class = in,
				 ttl = 176800,
				 rdata = ?PC}], <<>>} == 
	      parse_resource_records(1, <<?SPCB_/binary, 5:16, 1:16, 176800:32, 
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

%% @doc Return one random item out of a list.
%% @private Internal helper function.
%% @since 0.2
one_of(L) ->
  lists:nth(random:uniform(length(L)), L).

%% @doc Return N random items out of a list.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2
n_of(0, _L) ->
  [];
n_of(N, L) ->
  if length(L) < N -> L;
     true -> [one_of(L)|n_of(N - 1, L)]
  end.

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