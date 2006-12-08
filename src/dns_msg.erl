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

-export([parse_message/1]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% @ doc Structure defining a DNS message. It is based on what is defined on RFC1035 <http://www.ietf.org/rfc/rfc1035.txt> but it has been re-arranged for easy of use and some fields than are not needed where removed (the counts, which can be calculated out of the length of the lists).
%% @ since 0.2.0
-record(dns_message, {
	  id,        % Assigned by the program that generates queries. This identifier is copied to the corresponding reply and can be used by the requester to match up replies to outstanding queries.
	  qr,        % Whether this message is a query false, or a response true.
	  opcode,    % Kind of query in this message.  This value is set by the originator of a query and copied into the response.
	  aa,        % The responding name server is an authority for the domain name in question section.
	  tc,        % Specifies that this message was truncated due to length greater than that permitted on the transmission channel.
	  rd,        % This may be set in a query and is copied into the response.  If RD is set, it directs the name server to pursue the query recursively.
	  ra,        % Is set or cleared in a response, and denotes whether recursive query support is available in the name server.
	  rcode,     % Type of the response.
	  question,  % The question for the name server.
	  answer,    % ResourceRecords answering the question.
	  authority, % ResourceRecords pointing toward an authority.
	  additional % ResourceRecords holding additional information.
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


%% @doc Given a binary representing a DNS message (incomming from the network) return the same DNS message represented as records.
%% @private Internal helper function.
%% @since 0.2.0
parse_message(RawMsg) ->
    %%io:fwrite("~w:parse_message(~w)~n", [?MODULE, RawMsg]),

    %% Separate header (in each of it fields) and body.
    <<ID:16, QR:1, OpCode:4, AA:1, TC:1, RD:1, RA:1, _Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Body/binary>> = RawMsg,

    %% TODO: catch or something the return of {error, invalid} to return {error, invalid} from any of the parsing functions.
    %% Parse the questions and each of the other resource record sections.
    {questions, Questions, Rest} = parse_questions(QDCOUNT, Body),
    {resource_records, Answer, Rest2} = parse_resource_records(ANCOUNT, Rest),
    {resource_records, Authority, Rest3} = parse_resource_records(NSCOUNT, Rest2),
    {resource_records, Additional, _Rest4} = parse_resource_records(ARCOUNT, Rest3),

    %% Build the messag.
    #dns_message{id = ID, qr = parse_qr(QR), opcode = parse_opcode(OpCode),
                 aa = parse_bool(AA), tc = parse_bool(TC), rd = parse_bool(RD),
                 ra = parse_bool(RA), rcode = parse_rcode(RCODE), question = Questions,
                 answer = Answer, authority = Authority, additional = Additional}.


%% @doc Parse the query section of a DNS message.
%%      Returns {questions, Questions, Rest} where Question is the list of parsed questions and Rest is the rest of the binary message that was not parsed.
%% @spec parse_questions(integer(), binary()) -> {questions, Questions, Rest} | {error, invalid}
%% @private Internal helper function.
%% @since 0.2.0
parse_questions(Count, RawBody) -> parse_questions(Count, RawBody, []).

parse_questions(0, Rest, Questions) -> {questions, lists:reverse(Questions), Rest};
parse_questions(Count, RawBody, Questions) ->
    %% To parse a question, first parse the domain. T
    case parse_domain(RawBody) of
        {domain, QNAME, <<RawQTYPE:16, RawQCLASS:16, Rest/binary>>} ->
            case parse_qtype(RawQTYPE) of
                {qtype, QTYPE} ->
                    parse_questions(Count - 1, Rest,
                                    [#question{qname = QNAME, qtype = QTYPE, qclass = parse_qclass(RawQCLASS)}|
                                     Questions]);
                {error, invalid} ->
                    {error, invalid}
            end;
        {error, invalid} ->
            {error, invalid}
    end.


%% @doc Unparse the query section of a DNS message. From records, build the binaries.
%% @private Internal helper function.
%% @since 0.2.0
unparse_questions({questions, Questions, Rest}) ->
    RawQuestions = unparse_questions(Questions),
    <<RawQuestions/binary, Rest/binary>>;
unparse_questions(Questions) -> unparse_questions(<<>>, Questions).

unparse_questions(RawQuestions, []) -> RawQuestions;
unparse_questions(RawQuestions, [#question{qname=QName, qtype=QType, qclass=QClass}|Questions]) ->
    {unparse_domain, RawQName} = unparse_domain(QName),
    RawQType = unparse_qtype(QType),
    RawQClass = unparse_qclass(QClass),
    unparse_questions(<<RawQuestions/binary,
                       RawQName/binary,
                       RawQType/binary,
                       RawQClass/binary>>, Questions).

%% @doc Parse the resource records.
%% @private Internal helper function.
%% @since 0.2.0
parse_resource_records(Count, Body) ->
    %%io:fwrite("~w:parse_resource_records(~w, ~w)~n", [?MODULE, Count, Body]),
    parse_resource_records(Count, Body, []).

parse_resource_records(0, Body, RRs) ->
    %%io:fwrite("~w:parse_resource_records(~w, ~w, ~w)~n", [?MODULE, 0, Body, RRs]),
    {resource_records, lists:reverse(RRs), Body};
parse_resource_records(Count, Body, RRs) ->
    %%io:fwrite("~w:parse_resource_records(~w, ~w, ~w)~n", [?MODULE, Count, Body, RRs]),
    %% Parse the domain part and match all the other fields.
    {domain,
     Name,
     <<RawType:16, RawClass:16, TTL:32,
      RDLength:16, RawRData:RDLength/binary, Rest/binary>>} = parse_domain(Body),
    Type = parse_type(RawType),
    Class = parse_class(RawClass),
    {rdata, RData} = parse_rdata(Type, RawRData),
    parse_resource_records(Count - 1, Rest,
                           [#resource_record{name = Name, type = Type,
                                             class = Class, ttl = TTL,
                                             rdata = RData}            | RRs]).

%% @doc Unparse resource records.
%% @private Internal helper function.
%% @since 0.2.0
unparse_resource_record({resource_records, RRs, Rest}) ->
    RawRRs = unparse_resource_record(RRs),
    <<RawRRs/binary, Rest/binary>>;
unparse_resource_record(RRs) -> unparse_resource_record(<<>>, RRs).

unparse_resource_record(RawRRs, []) -> RawRRs;
unparse_resource_record(RawRRs, [#resource_record{name=Name, type=Type, class=Class, ttl=TTL, rdata=RData}|RRs]) ->
    RawName = unparse_domain(Name),
    RawType = unparse_type(Type),
    RawClass = unparse_class(Class),
    RawRData = unparse_rdata(Type, RData),
    RawRDLength = length(RawRData),
    unparse_resource_record(<<RawRRs/binary,
                             RawName/binary,
                             RawType/binary,
                             RawClass/binary,
                             TTL:32,
                             RawRDLength:16,
                             RawRData/binary>>, RRs).

%% @doc Parse RDATA, the data of a resource record.
%% @private Internal helper function.
%% @since 0.2.0
parse_rdata(a,     _RawRData) -> unspecified;
parse_rdata(ns,    _RawRData) -> unspecified;
parse_rdata(md,    _RawRData) -> unspecified;
parse_rdata(mf,    _RawRData) -> unspecified;
parse_rdata(cname, RawRData) ->
    {domain, Domain, _Rest} = parse_domain(RawRData),
    {rdata, Domain};
parse_rdata(soa,   _RawRData) -> unspecified;
parse_rdata(mb,    _RawRData) -> unspecified;
parse_rdata(mg,    _RawRData) -> unspecified;
parse_rdata(mr,    _RawRData) -> unspecified;
parse_rdata(null,  _RawRData) -> unspecified;
parse_rdata(wks,   _RawRData) -> unspecified;
parse_rdata(ptr,   _RawRData) -> unspecified;
parse_rdata(hinfo, _RawRData) -> unspecified;
parse_rdata(minfo, _RawRData) -> unspecified;
parse_rdata(mx,    _RawRData) -> unspecified;
parse_rdata(_Type, _RawRData) ->
    {error, invalid}.

%% @doc Unparse RDATA, the data of a resource record.
%% @private Internal helper function.
%% @since 0.2.0
unparse_rdata(a,     _RawRData) -> <<>>;
unparse_rdata(ns,    _RawRData) -> <<>>;
unparse_rdata(md,    _RawRData) -> <<>>;
unparse_rdata(mf,    _RawRData) -> <<>>;
unparse_rdata(cname, RawRData) -> 
    {raw_domain, RawDomain, _Rest} = unparse_domain(RawRData),
    {rdata, RawDomain};
unparse_rdata(soa,   _RawRData) -> <<>>;
unparse_rdata(mb,    _RawRData) -> <<>>;
unparse_rdata(mg,    _RawRData) -> <<>>;
unparse_rdata(mr,    _RawRData) -> <<>>;
unparse_rdata(null,  _RawRData) -> <<>>;
unparse_rdata(wks,   _RawRData) -> <<>>;
unparse_rdata(ptr,   _RawRData) -> <<>>;
unparse_rdata(hinfo, _RawRData) -> <<>>;
unparse_rdata(minfo, _RawRData) -> <<>>;
unparse_rdata(mx,    _RawRData) -> <<>>;
unparse_rdata(_Type, _RawRData) -> 
    {error, invalid}.

%% @doc Parse a DNS domain.
%% @private Internal helper function.
%% @since 0.2.0
parse_domain(Body) -> parse_domain([], Body).
parse_domain(Labels, <<Length:8, Label:Length/binary-unit:8, Rest/binary>>) when Length > 0 ->
    parse_domain([binary_to_list(Label)|Labels], Rest);
parse_domain(Labels, <<Length:8, Rest/binary>>) when Length == 0 ->
    {domain, lists:reverse(Labels), Rest};
parse_domain(_Labels, _Body) ->
    {error, invalid}.

%% @doc Unparse a DNS domain.
%% @private Internal helper function.
%% @since 0.2.0
unparse_domain({domain, Domain, Rest}) ->
    {raw_domain, RawDomain} = unparse_domain(Domain),
    {raw_domain, <<RawDomain/binary, Rest/binary>>};
unparse_domain(Domain) -> {raw_domain, unparse_domain(<<>>, Domain)}.

unparse_domain(RawDomain, []) -> <<RawDomain/binary, 0:8>>;
unparse_domain(RawDomain, [Label|Labels]) ->
    LabelLength = length(Label),
    BinaryLabel = list_to_binary(Label),
    unparse_domain(<<RawDomain/binary, LabelLength:8, BinaryLabel/binary>>, Labels).

%% @doc Turn a numeric DNS type into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_type(1)  -> {type, a};
parse_type(2)  -> {type, ns};
parse_type(3)  -> {type, md};
parse_type(4)  -> {type, mf};
parse_type(5)  -> {type, cname};
parse_type(6)  -> {type, soa};
parse_type(7)  -> {type, mb};
parse_type(8)  -> {type, mg};
parse_type(9)  -> {type, mr};
parse_type(10) -> {type, null};
parse_type(11) -> {type, wks};
parse_type(12) -> {type, ptr};
parse_type(13) -> {type, hinfo};
parse_type(14) -> {type, minfo};
parse_type(15) -> {type, mx};
parse_type(16) -> {type, txt};
parse_type(_) ->  {error, invalid}.

%% @doc Unparse a DNS type.
%% @private Internal helper function.
%% @since 0.2.0
unparse_type(a) ->     {raw_type, << 1:16>>};
unparse_type(ns) ->    {raw_type, << 2:16>>};
unparse_type(md) ->    {raw_type, << 3:16>>};
unparse_type(mf) ->    {raw_type, << 4:16>>};
unparse_type(cname) -> {raw_type, << 5:16>>};
unparse_type(soa) ->   {raw_type, << 6:16>>};
unparse_type(mb) ->    {raw_type, << 7:16>>};
unparse_type(mg) ->    {raw_type, << 8:16>>};
unparse_type(mr) ->    {raw_type, << 9:16>>};
unparse_type(null) ->  {raw_type, <<10:16>>};
unparse_type(wks) ->   {raw_type, <<11:16>>};
unparse_type(ptr) ->   {raw_type, <<12:16>>};
unparse_type(hinfo) -> {raw_type, <<13:16>>};
unparse_type(minfo) -> {raw_type, <<14:16>>};
unparse_type(mx) ->    {raw_type, <<15:16>>};
unparse_type(txt) ->   {raw_type, <<16:16>>};
unparse_type(_) ->     {error, invalid}.

%% @doc Turn a numeric DNS qtype into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_qtype(252) -> {qtype, axfr};
parse_qtype(253) -> {qtype, mailb};
parse_qtype(254) -> {qtype, maila};
parse_qtype(255) -> {qtype, all};
parse_qtype(RawQType) ->
    case parse_type(RawQType) of
        {type, QType} -> {qtype, QType};
        {error, invalid} -> {error, invalid}
    end.

%% @doc Unparse a DNS qtype.
%% @private Internal helper function.
%% @since 0.2.0
unparse_qtype(axfr) ->  {raw_qtype, <<252:16>>};
unparse_qtype(mailb) -> {raw_qtype, <<253:16>>};
unparse_qtype(maila) -> {raw_qtype, <<254:16>>};
unparse_qtype(all) ->   {raw_qtype, <<255:16>>};
unparse_qtype(QType) ->
    case unparse_type(QType) of
        {raw_type, RawQType} -> {raw_qtype, RawQType};
        {error, invalid} -> {error, invalid}
    end.

%% @doc Turn a numeric DNS class into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_class(1) -> in;
parse_class(2) -> cs;
parse_class(3) -> ch;
parse_class(4) -> hs;
parse_class(_) -> {error, invalid}.

%% @doc Unparse a DNS class.
%% @private Internal helper function.
%% @since 0.2.0
unparse_class(in) -> <<1:16>>;
unparse_class(cs) -> <<2:16>>;
unparse_class(ch) -> <<3:16>>;
unparse_class(hs) -> <<4:16>>;
unparse_class(_)  -> {error, invalid}.

%% @doc Turn a numeric DNS qclass into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_qclass(255) -> any;
parse_qclass(Class) -> parse_class(Class).

%% @doc Unparse a DNS qclass.
%% @private Internal helper function.
%% @since 0.2.0
unparse_qclass(any) -> <<255:16>>;
unparse_qclass(Class) -> unparse_class(Class).

%% @doc Parse a DNS qr.
%% @private Internal helper function.
%% @since 0.2.0
parse_qr(0) -> {qr, query_};
parse_qr(1) -> {qr, response};
parse_qr(_) -> {error, invalid}.

%% @doc Parse a DNS qr.
%% @private Internal helper function.
%% @since 0.2.0
unparse_qr(query_)   -> {raw_qr, 0};
unparse_qr(response) -> {raw_qr, 1};
unparse_qr(_) ->        {error, invalid}.

%% @doc Parse an opcode.
%% @private Internal helper function.
%% @since 0.2.0
parse_opcode(0) -> {opcode, query_};
parse_opcode(1) -> {opcode, iquery};
parse_opcode(2) -> {opcode, status};
parse_opcode(_) -> {error, invalid}.

%% @doc Unpparse an opcode.
%% @private Internal helper function.
%% @since 0.2.0
unparse_opcode(query_) -> {raw_opcode, 0};
unparse_opcode(iquery) -> {raw_opcode, 1};
unparse_opcode(status) -> {raw_opcode, 2};
unparse_opcode(_) ->      {error, invalid}.

%% @doc Parse DNS RCodes.
%% @private Internal helper function.
%% @since 0.2.0
parse_rcode(0) -> {rcode, no_error};
parse_rcode(1) -> {rcode, format_error};
parse_rcode(2) -> {rcode, server_failure};
parse_rcode(3) -> {rcode, name_error};
parse_rcode(4) -> {rcode, not_implemented};
parse_rcode(5) -> {rcode, refused};
parse_rcode(_) -> {error, invalid}.

%% @doc Unparse DNS RCodes.
%% @private Internal helper function.
%% @since 0.2.0
unparse_rcode(no_error) ->        {raw_rcode, 0};
unparse_rcode(format_error) ->    {raw_rcode, 1};
unparse_rcode(server_failure) ->  {raw_rcode, 2};
unparse_rcode(name_error) ->      {raw_rcode, 3};
unparse_rcode(not_implemented) -> {raw_rcode, 4};
unparse_rcode(refused) ->         {raw_rcode, 5};
unparse_rcode(_) ->               {error, invalid}.

%% @doc Parse boolean values.
%% @private Internal helper function.
%% @since 0.2.0
parse_bool(0) -> {bool, false};
parse_bool(1) -> {bool, true};
parse_bool(_) -> {error, invalid}.

%% @doc Unparse boolean values.
%% @private Internal helper function.
%% @since 0.2.0
unparse_bool(false) -> {raw_bool, 0};
unparse_bool(true) ->  {raw_bool, 1};
unparse_bool(_) ->     {error, invalid}.

%% @doc true if the argument is an error, non-true otherwise (false).
%% @private Internal helper function.
%% @since 0.2.0
is_error({error, _}) -> true;
is_error(_) -> false.

%% @doc If there's no error in the Results, then it renturns {no_error, Results}. If there's at least one error, it'll combine all errors into {error, Reasons} where Reasons is a list of all the reasons of each present error.
%% @private Internal helper function.
%% @since 0.2.0
any_error(Results) ->
    Errors = lists:filter(fun is_error/1, Results),
    if length(Errors) == 0 -> {no_error, Results};
       true -> {error, lists:map(fun({error, Reason}) -> Reason end, Errors)}
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

%%% Testing data

%% Some labels (atoms of domain names) to test the parser.
-define(LABELS, [{correct, ["com"], <<3, "com">>},
  		 {correct, ["s"], <<1, "s">>},
 		 %%{correct, ["abcdefghijklmnopqrstuvwxyz-0123456789-abcdefghijklmnopqrstuvwxy"], <<63, "abcdefghijklmnopqrstuvwxyz-0123456789-abcdefghijklmnopqrstuvwxy">>},
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
-define(QRS, [{correct, query_,   0},
 	      {correct, response, 1},
              {error,   err,      2}]).

%% DNS OpCodes to test the parser.
-define(OPCODES, [{correct, query_, 0},
 		  {correct, iquery, 1},
 		  {correct, status, 2},
                  {error,   err,    5}]).

%% DNS RCodes to test the parser.
-define(RCODES, [{correct, no_error,        0},
 		 {correct, format_error,    1},
 		 {correct, server_failure,  2},
 		 {correct, name_error,      3},
 		 {correct, not_implemented, 4},
 		 {correct, refused,         5},
                 {error,   err,            10}]).

%% DNS Booleans to test the parser.
-define(BOOLEANS, [{correct, false, 0},
 		   {correct, true,  1},
                   {error,   err,  10}]).

%% @doc Generates and run all tests.
%% @since 0.2.0
all_test_() ->
    Factor = 4,
    Sample = 40,

    Domains = build_domains(?LABELS, Factor, Sample), %% Build the domains and take a sample of it.
    DomainParsingTests = domain_parsing_tests(Domains),
    DomainUnparsingTests = domain_unparsing_tests(Domains),

    Questions = build_questions(Domains, ?QTYPES, ?QCLASSES, Factor, Sample),
    QuestionsParsingTests = questions_parsing_tests(Questions),
    %%QuestionsUnparsingTests = questions_unparsing_tests(Questions),

    %% TODO: make these tests dynamic as the previous ones.
    %%Types = n_of(Sample, ?TYPES),
    %%Classes = n_of(Sample, ?CLASSES),
    %%RRs = n_of(Sample, build_resource_records(Domains, Types, Classes, Factor)),
%%    RRsParsingTests = tests_resource_record_parsing(),

    %% TODO: make these tests dynamic as the previous ones.
%%    MessageParsingTests = tests_message_parsing(),

    %%DomainParsingTests ++ DomainUnparsingTests ++
    %%QuestionsParsingTests. %%, QuestionsUnparsingTests.
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Domain Parsing and Unparsing testing %%%%%%

%% @doc Having a list of Domains build all the parsing tests to be used by EUnit.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2.0
domain_parsing_tests([]) -> [];
domain_parsing_tests([{Type, Parsed, Raw}|Domains]) ->
    Noise = list_to_binary(noise()),
    CRaw = <<Raw/binary, Noise/binary>>,         % Complete raw, add noise.
    CParsed = {domain, Parsed, Noise},           % Complete parsed, add signature and noise.
    ParsedToTest = (catch parse_domain(CRaw)),   % Perform the parsing.
    Desc = lists:flatten(                        % Some useful description
             io_lib:format("~p, ~p, ~p, ~p", [Type, CParsed, CRaw, ParsedToTest])),
    [{Desc, case Type of                                           % What kind of test is it ?
                correct ->
                    ?_assert(ParsedToTest == CParsed);
                error   ->
                    ?_assert((ParsedToTest == {error, invalid}) or % We should get an error
                             (ParsedToTest /= CParsed))            % or plain wrong data (not an exception).
            end} | domain_parsing_tests(Domains)].

%% @doc Having a list of Domains build all the unparsing tests to be used by EUnit.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2.0
domain_unparsing_tests([]) -> [];
domain_unparsing_tests([{Type, Parsed, Raw}|Domains]) ->
    Noise = list_to_binary(noise()),
    CRaw = <<Raw/binary, Noise/binary>>,         % Complete raw, add noise
    CParsed = {domain, Parsed, Noise},           % Complete parsed, add signature and noise.
    {raw_domain, RawToTest} = (catch unparse_domain(CParsed)), % Perform the unparsing.
    Desc = lists:flatten(                        % Some useful description
             io_lib:format("~w, ~w, ~w, ~w", [Type, CParsed, CRaw, RawToTest])),
    [{Desc, case Type of                                        % What kind of test is it ?
                correct ->
                    ?_assert(RawToTest == CRaw);
                error   ->
                    ?_assert((RawToTest == {error, invalid}) or % We should get an error
                             (RawToTest /= CRaw))               % or plain wrong data (not an exception).
            end} | domain_unparsing_tests(Domains)].

%% @doc Using lables Labels, build all possible domains from those of length 1 to length Length.
%% @private Internal helper function.
%% @since 0.2.0
build_domains(Labels, Length, Sample) ->
    SampleOfLabels = n_of(Sample, Labels),
    BuildDomains = fun(N, Domains) ->                              % Function to build domains of N length and append it to Domains.
                           Domains ++ build_domains_(SampleOfLabels, N, Sample)
                   end,
    Domains = lists:foldl(BuildDomains, [], lists:seq(1, Length)), % Build all possible domains up to Length.
    NullTerm = fun({Type, ParsedDomain, RawDomain}) ->             % Function to add the null character to the domain.
                       {Type, ParsedDomain, <<RawDomain/binary, 0>>}
               end,
    n_of(Sample, lists:map(NullTerm, Domains)).                    % Add the null character to the end of each domains.

%% @doc Having a set of labels build domains names of N labels.
%% @private Internal helper function.
%% @todo Find a better name for this function.
%% @since 0.2.0
build_domains_(_Labels, 0, _Sample) -> [];
build_domains_(Labels, 1, Sample) -> n_of(Sample, Labels);
build_domains_(Labels, Length, Sample) ->
    Domains = n_of(Sample, build_domains_(Labels, Length - 1, Sample)),
    Comb = fun(Label) ->                        % Function to combine one label to NewDomains.
                   one_domain_per_domain(Label, Domains)
           end, 
    n_of(10, lists:flatten(lists:map(Comb, Labels))).

%% @doc Having one label combine it with each domain of a list.
%% @private Internal helper function.
%% @since 0.2.0
one_domain_per_domain({Type, Parsed, Raw}, Domains) ->
    Comb = fun({Type2, Parsed2, Raw2}) ->  % Function to combine two domains.
                   if (Type == correct) and (Type2 == correct) ->
                           {correct, lists:append(Parsed, Parsed2), <<Raw/binary, Raw2/binary>>};
                      true ->
                           {error, lists:append(Parsed, Parsed2), <<Raw/binary, Raw2/binary>>}
                   end
           end,
    lists:map(Comb, Domains).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Question Parsing and Unparsing testing %%%%%

%% @doc Having a list of Questions build the parsing tests.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2.0
questions_parsing_tests([]) -> [];
questions_parsing_tests([{Type, Count, Parsed, Raw}|Questions]) ->
    Noise = list_to_binary(noise()),
    CRaw = <<Raw/binary, Noise/binary>>,                 % Complete raw, add noise.
    CParsed = {questions, Parsed, Noise},                % Complete parsed, add signature and noise.
    ParsedToTest = (catch parse_questions(Count, CRaw)), % Perform the parsing.
    Desc = lists:flatten(                                % Some useful description
             io_lib:format("~p, ~p, ~p, ~p, ~p", [Type, CParsed, Count, CRaw, ParsedToTest])),
    [{Desc, case Type of                                           % What kind of test is it ?
                correct ->
                    ?_assert(ParsedToTest == CParsed);
                error   ->
                    ?_assert((ParsedToTest == {error, invalid}) or % We should get an error
                             (ParsedToTest /= CParsed))            % or plain wrong data (not an exception).
            end} | questions_parsing_tests(Questions)].

%% @doc Having a list of Questions build the unparsing tests.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2.0
questions_unparsing_tests([]) -> [];
questions_unparsing_tests([{Type, _Count, Parsed, Raw}|Questions]) ->
    Noise = list_to_binary(noise()),
    CRaw = <<Raw/binary, Noise/binary>>,            % Complete raw, add noise.
    CParsed = {questions, Parsed, Noise},           % Complete parsed, add signature and noise.
    RawToTest = (catch unparse_questions(CParsed)), % Perform the unparsing.
    Desc = lists:flatten(                           % Some useful description
             io_lib:format("~p, ~p, ~p, ~p", [Type, CParsed, CRaw, RawToTest])),
    [{Desc, case Type of                                        % What kind of test is it ?
                correct ->
                    ?_assert(RawToTest == CRaw);
                error   ->
                    ?_assert((RawToTest == {error, invalid}) or % We should get an error
                             (RawToTest /= CRaw))               % or plain wrong data (not an exception).
            end} | questions_unparsing_tests(Questions)].

%% @doc Using domains Domains, QTypes and QClasses build all possible questions up to length Length (that is, chained questions). 
%% @private Internal helper function.
%% @since 0.2.0
build_questions(Domains, QTypes, QClasses, Length, Sample) ->
    Questions = n_of(Sample, build_questions(Domains, QTypes, QClasses, Sample)),
    BuildQuestions = fun(N, NewQuestions) ->                           % Function to build questions of N length and append it to NewQuestions.
                             NewQuestions ++ build_questions(Questions, N, Sample)
                     end,
    n_of(Sample, lists:foldl(BuildQuestions, [], lists:seq(1, Length))).

%% @doc Make all the possible combinations for a set of Domains, QTypes and QClasses. 
%% @private Internal helper function.
%% @since 0.2.0
build_questions([], _QTypes, _QClasses, _Sample) -> [];
build_questions(_Domains, [], _QClasses, _Sample) -> [];
build_questions(_Domains, _QTypes, [], _Sample) -> [];
build_questions({DType, DParsed, DRaw}, {QTType, QTParsed, QTRaw}, {QCType, QCParsed, QCRaw}, _Sample) ->
    Type = if (DType == correct) and (QTType == correct) and (QCType == correct) -> correct;
              true -> error
           end,
    {Type, 1,
     [#question{qname = DParsed, qtype = QTParsed, qclass = QCParsed}],
     <<DRaw/binary, QTRaw/binary, QCRaw/binary>>};
build_questions({DType, DParsed, DRaw}, {QTType, QTParsed, QTRaw}, QClasses, Sample) ->
    BuildQuestion = fun(QClass) ->
                            Sample, build_questions({DType, DParsed, DRaw},
                                                    {QTType, QTParsed, QTRaw},
                                                    QClass, Sample)
                    end,
    n_of(Sample, lists:flatten(lists:map(BuildQuestion, QClasses)));
build_questions({DType, DParsed, DRaw}, QTypes, QClasses, Sample) ->
    BuildQuestion = fun(QType) ->
                            n_of(Sample, build_questions({DType, DParsed, DRaw},
                                                         QType, QClasses, Sample))
                    end,
    lists:flatten(lists:map(BuildQuestion, QTypes));
build_questions(Domains, QTypes, QClasses, Sample) ->
    BuildQuestion = fun(Domain) ->
                            n_of(Sample, build_questions(Domain, QTypes, QClasses, Sample))
                    end,
    n_of(Sample, lists:flatten(lists:map(BuildQuestion, Domains))).

%% @doc Combine each question in Question with every other item in Questions up to N. 
%% @private Internal helper function.
%% @since 0.2.0
build_questions(_Questions, 0, _Sample) -> [];
build_questions(Questions, 1, Sample) -> n_of(Sample, Questions);
build_questions(Questions, N, Sample) ->
    NewQuestions = n_of(Sample, build_questions(Questions, N - 1, Sample)),
    Comb = fun(Question) ->              % Function to combine one question to NewQuestions.
                   one_question_per_questions(Question, NewQuestions) end, 
    n_of(Sample, lists:flatten(lists:map(Comb, Questions))).

%% @doc Combine one question with every other list of questions (in Questions).
%% @private Internal helper function.
%% @since 0.2.0
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% Resource Record Parsing and Unparsing Testing %%

%% %% @doc . 
%% %% @private Internal helper function.
%% %% @since 0.2.0
%% %%build_resource_records(_Domains, _Types, _Classes, _Length) ->
%%   %%  RDATA = [{cname, Domains},
%%   %% 	   {hinfo, []},    % Where do we get random hinfos ?
%%   %% 	   {mx, []},       % Build MX using Domains and a random 16-bits integer.
%%   %% 	   {ns, Domains},
%%   %% 	   {ptr, Domains},
%%   %% 	   {soa, []},      % Build SOA using Domains, root+domains for emails and random values.
%%   %% 	   {txt, []},      % Build txt with random text.
%%   %% 	   {a, []},        % Where do we get random IPs ?
%%   %% 	   {wks, []}],     % is this used anyway ?
%%   %%[].

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%% DNS Message Parsing testing %%%%%%%%%%%

%% %% @doc . 
%% %% @private Internal helper function.
%% %% @since 0.2.0
%% %%build_messages(_Questions, _RRs) ->
%% %%  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Qr Parsing and Unparsing testing %%%%%%

qr_parsing_test_() -> qr_parsing_tests(?QRS).
qr_parsing_tests([]) -> [];
qr_parsing_tests([{Type, Parsed, Raw}|Qrs]) ->
    ParsedToTest = parse_qr(Raw), % Perform the parsing.
    Desc = lists:flatten(            % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == {qr, Parsed});
                error   -> ?_assert(ParsedToTest == {error, invalid}) % We should get an error.
            end} | qr_parsing_tests(Qrs)].

qr_unparsing_test_() -> qr_unparsing_tests(?QRS).
qr_unparsing_tests([]) -> [];
qr_unparsing_tests([{Type, Parsed, Raw}|Qrs]) ->
    RawToTest = unparse_qr(Parsed), % Perform the parsing.
    Desc = lists:flatten(                % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, RawToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(RawToTest == {raw_qr, Raw});
                error   -> ?_assert(RawToTest == {error, invalid}) % We should get an error.
            end} | qr_unparsing_tests(Qrs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% OpCode Parsing and Unparsing testing %%%%%%

opcode_parsing_test_() -> opcode_parsing_tests(?OPCODES).
opcode_parsing_tests([]) -> [];
opcode_parsing_tests([{Type, Parsed, Raw}|OpCodes]) ->
    ParsedToTest = parse_opcode(Raw), % Perform the parsing.
    Desc = lists:flatten(            % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == {opcode, Parsed});
                error   -> ?_assert(ParsedToTest == {error, invalid}) % We should get an error.
            end} | opcode_parsing_tests(OpCodes)].

opcode_unparsing_test_() -> opcode_unparsing_tests(?OPCODES).
opcode_unparsing_tests([]) -> [];
opcode_unparsing_tests([{Type, Parsed, Raw}|OpCodes]) ->
    RawToTest = unparse_opcode(Parsed), % Perform the parsing.
    Desc = lists:flatten(                % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, RawToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(RawToTest == {raw_opcode, Raw});
                error   -> ?_assert(RawToTest == {error, invalid}) % We should get an error.
            end} | opcode_unparsing_tests(OpCodes)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% RCode Parsing and Unparsing testing %%%%%%

rcode_parsing_test_() -> rcode_parsing_tests(?RCODES).
rcode_parsing_tests([]) -> [];
rcode_parsing_tests([{Type, Parsed, Raw}|RCodes]) ->
    ParsedToTest = parse_rcode(Raw), % Perform the parsing.
    Desc = lists:flatten(            % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == {rcode, Parsed});
                error   -> ?_assert(ParsedToTest == {error, invalid}) % We should get an error.
            end} | rcode_parsing_tests(RCodes)].

rcode_unparsing_test_() -> rcode_unparsing_tests(?RCODES).
rcode_unparsing_tests([]) -> [];
rcode_unparsing_tests([{Type, Parsed, Raw}|RCodes]) ->
    RawToTest = unparse_rcode(Parsed), % Perform the parsing.
    Desc = lists:flatten(                % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, RawToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(RawToTest == {raw_rcode, Raw});
                error   -> ?_assert(RawToTest == {error, invalid}) % We should get an error.
            end} | rcode_unparsing_tests(RCodes)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Bool Parsing and Unparsing testing %%%%%%

bool_parsing_test_() -> bool_parsing_tests(?BOOLEANS).
bool_parsing_tests([]) -> [];
bool_parsing_tests([{Type, Parsed, Raw}|Bools]) ->
    ParsedToTest = parse_bool(Raw),  % Perform the parsing.
    Desc = lists:flatten(            % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == {bool, Parsed});
                error   -> ?_assert(ParsedToTest == {error, invalid}) % We should get an error.
            end} | bool_parsing_tests(Bools)].

bool_unparsing_test_() -> bool_unparsing_tests(?BOOLEANS).
bool_unparsing_tests([]) -> [];
bool_unparsing_tests([{Type, Parsed, Raw}|Bools]) ->
    RawToTest = unparse_bool(Parsed), % Perform the parsing.
    Desc = lists:flatten(                % Some useful description.
             io_lib:format("~p, ~p, ~p, ~p", [Type, Parsed, Raw, RawToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(RawToTest == {raw_bool, Raw});
                error   -> ?_assert(RawToTest == {error, invalid}) % We should get an error.
            end} | bool_unparsing_tests(Bools)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Error processing testing %%%%%%

is_error_test_() ->
    [?_assert(is_error({error, invalid}) == true),
     ?_assert(is_error({error, whatever}) == true),
     ?_assert(is_error(whatever) /= true)].

any_error_test_() ->
    A = {type_a, value_a},
    B = {type_b, value_b},
    [?_assert(any_error([A, B]) == {no_error, [A, B]}),
     ?_assert(any_error([A, {error, invalid}]) == {error, [invalid]}),
     ?_assert(any_error([{error, invalid}, {error, whatever}]) == {error, [invalid, whatever]})].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Helper functions for testing %%%%%%

%% @doc Return one random item out of a list.
%% @private Internal helper function.
%% @since 0.2.0
one_of(L) ->
    lists:nth(random:uniform(length(L)), L).

%% @doc Return N random items out of a list.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2.0
n_of(all, L) -> L;
n_of(N, L) ->
    if length(L) < N -> L;
       true -> n_of(N, L, [])
    end.
n_of(0, _L, NL) ->
    NL;
n_of(N, L, NL) ->
    if length(L) =< N -> L;
       true -> n_of(N - 1, L, [one_of(L)|NL])
    end.

%% @doc Generate some noise, that is a list of random length (less than 15) with random data.
%%      The porpuse is to insert data in places where the system should not look at.
%% @private Internal helper function.
%% @since 0.2.0
noise() ->
    {I1,I2,I3} = erlang:now(),
    random:seed(I1,I2,I3),
    noise(random:uniform(15) - 1).

noise(0) -> [];
noise(Max) ->
    [random:uniform(256) - 1 |
     noise(Max - 1)].

%% %%%%%%%%%%%%%%%%%% Old boring tests %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -define(C, ["com"]).
%% -define(CB, <<3, "com">>).
%% -define(PC, ["pupeno"|?C]).
%% -define(PCB, <<6, "pupeno", ?CB/binary>>).
%% -define(SPC, ["software"|?PC]).
%% -define(SPCB, <<8, "software", ?PCB/binary>>).
%% -define(C_ALL_IN, #question{qname = ?C, qtype = all, qclass = in}).
%% -define(C_ALL_INB, <<?CB/binary, 0, 255:16, 1:16>>).
%% -define(C_MX_CS, #question{qname = ?C, qtype = mx, qclass = cs}).
%% -define(C_MX_CSB, <<?CB/binary, 0, 15:16, 2:16>>).
%% -define(PC_NS_CH, #question{qname = ?PC, qtype = ns, qclass = ch}).
%% -define(PC_NS_CHB, <<?PCB/binary, 0, 2:16, 3:16>>).
%% -define(PC_SOA_HS, #question{qname = ?PC, qtype = soa, qclass = hs}).
%% -define(PC_SOA_HSB, <<?PCB/binary, 0, 6:16, 4:16>>).
%% -define(SPC_A_ANY, #question{qname = ?SPC, qtype = a, qclass = any}).
%% -define(SPC_A_ANYB, <<?SPCB/binary, 0, 1:16, 255:16>>).
%% -define(SPC_PTR_IN, #question{qname = ?SPC, qtype = ptr, qclass = in}).
%% -define(SPC_PTR_INB, <<?SPCB/binary, 0, 12:16, 1:16>>).

%% -define(PCB_, <<?PCB/binary, 0:8>>).
%% -define(SPCB_, <<?SPCB/binary, 0:8>>).
%% tests_resource_record_parsing() ->
%%   PCB_L = length(binary_to_list(?PCB_)),
%%   [{"No RR", ?_assert({resource_records, [], <<>>} == parse_resource_records(0, <<>>))},
%%    {"One RR", 
%%     [?_assert({resource_records,
%% 	       [#resource_record{name = ?SPC,
%% 				 type = cname,
%% 				 class = in,
%% 				 ttl = 176800,
%% 				 rdata = ?PC}],
%% 	       <<>>} == 
%% 	      parse_resource_records(1, <<?SPCB_/binary, 5:16, 1:16, 176800:32, 
%% 					 PCB_L:16, ?PCB_/binary>>))]}].

%% tests_message_parsing() ->
%%   [?_assert(#dns_message{id = 63296, 
%% 			 qr = query_,
%% 			 opcode = query_,
%% 			 aa = false,
%% 			 tc = false,
%% 			 rd = true,
%% 			 ra = false,
%% 			 rcode = no_error,
%% 			 question = [],
%% 			 answer = [],
%% 			 authority = [],
%% 			 additional = []} ==
%% 	    parse_message(<<63296:16, 0:1, 0:4, 0:1, 0:1, 1:1, 0:1, 0:3, 0:4,
%% 			   0:16, 0:16, 0:16, 0:16>>)),
%%    ?_assert(#dns_message{id = 13346, 
%% 			 qr = response,
%% 			 opcode = status,
%% 			 aa = true,
%% 			 tc = true,
%% 			 rd = false,
%% 			 ra = true,
%% 			 rcode = server_failure,
%% 			 question = [],
%% 			 answer = [],
%% 			 authority = [],
%% 			 additional = []} ==
%% 	    parse_message(<<13346:16, 1:1, 2:4, 1:1, 1:1, 0:1, 1:1, 0:3, 2:4,
%% 			   0:16, 0:16, 0:16, 0:16>>)),
%%    ?_assert(#dns_message{id = 63296, 
%% 			 qr = query_,
%% 			 opcode = iquery,
%% 			 aa = false,
%% 			 tc = false,
%% 			 rd = true,
%% 			 ra = false,
%% 			 rcode = refused,
%% 			 question = [?C_ALL_IN],
%% 			 answer = [],
%% 			 authority = [],
%% 			 additional = []} ==
%% 	    parse_message(<<63296:16, 0:1, 1:4, 0:1, 0:1, 1:1, 0:1, 0:3, 5:4,
%% 			   1:16, 0:16, 0:16, 0:16,?C_ALL_INB/binary >>)),
%%    ?_assert(#dns_message{id = 63296, 
%% 			 qr = query_,
%% 			 opcode = query_,
%% 			 aa = false,
%% 			 tc = false,
%% 			 rd = true,
%% 			 ra = false,
%% 			 rcode = format_error,
%% 			 question = [?C_ALL_IN, ?C_MX_CS, ?PC_NS_CH, 
%% 				     ?PC_SOA_HS, ?SPC_A_ANY, ?SPC_PTR_IN],
%% 			 answer = [],
%% 			 authority = [],
%% 			 additional = []} ==
%% 	    parse_message(<<63296:16, 0:1, 0:4, 0:1, 0:1, 1:1, 0:1, 0:3, 1:4,
%% 			   6:16, 0:16, 0:16, 0:16, ?C_ALL_INB/binary, ?C_MX_CSB/binary, 
%% 			   ?PC_NS_CHB/binary, ?PC_SOA_HSB/binary, ?SPC_A_ANYB/binary,
%% 			   ?SPC_PTR_INB/binary>>))].

-endif. %% ifdef(TEST).
