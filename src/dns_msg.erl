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

-define(INVALIDCATCH(NAME, BODY),
        try BODY
        catch
            error:{badmatch, Value} ->
                case Value of
                    {ivalid, Reason} -> {invalid, [NAME|Reason]};
                    _ -> {error, {unexpected_error, Value}}
                end
        end).

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
    ?INVALIDCATCH(
       raw_message,
       begin
           %% Separate header (in each of it fields) and body.
           <<ID:16, QR:1, OpCode:4, AA:1, TC:1, RD:1, RA:1, _Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, Body/binary>> = RawMsg,

           %% Parse the questions and each of the other resource record sections.
           {questions, Questions, Rest} = parse_questions(QDCOUNT, Body),
           {resource_records, Answer, Rest2} = parse_resource_records(ANCOUNT, Rest),
           {resource_records, Authority, Rest3} = parse_resource_records(NSCOUNT, Rest2),
           {resource_records, Additional, _Rest4} = parse_resource_records(ARCOUNT, Rest3),
           
           %% Build the messag.
           #dns_message{id = ID, qr = parse_qr(QR), opcode = parse_opcode(OpCode),
                        aa = parse_bool(AA), tc = parse_bool(TC), rd = parse_bool(RD),
                        ra = parse_bool(RA), rcode = parse_rcode(RCODE), question = Questions,
                        answer = Answer, authority = Authority, additional = Additional}
       end).


%% @doc Parse the query section of a DNS message.
%%      Returns {questions, Questions, Rest} where Question is the list of parsed questions and Rest is the rest of the binary message that was not parsed.
%% @spec parse_questions(integer(), binary()) -> {questions, Questions, Rest} | {invalid, Reason}
%% @private Internal helper function.
%% @since 0.2.0
parse_questions(Count, RawBody) -> parse_questions(Count, RawBody, []).

parse_questions(0, Rest, Questions) -> {questions, lists:reverse(Questions), Rest};
parse_questions(Count, RawBody, Questions) ->
    %%io:fwrite("~w:parse_questions(~p, ~p, ~p)~n", [?MODULE, Count, RawBody, Questions]),
    ?INVALIDCATCH(
       raw_question,
       begin
           %% To parse a question, first parse the domain.
           {domain, QName, <<RawQType:2/binary-unit:8, RawQClass:2/binary-unit:8, Rest/binary>>} = parse_domain(RawBody),
           {qtype, QType} = parse_qtype(RawQType),
           {qclass, QClass} = parse_qclass(RawQClass),
           Question = #question{qname = QName, qtype = QType, qclass = QClass},
           parse_questions(Count - 1, Rest, [Question|Questions])
       end).

%% @doc Unparse the query section of a DNS message. From records, build the binaries.
%% @private Internal helper function.
%% @since 0.2.0
unparse_questions({questions, Questions, Rest}) ->
    ?INVALIDCATCH(
       question,
       begin
           {raw_questions, RawQuestions} = unparse_questions(Questions),
           {raw_questions, <<RawQuestions/binary, Rest/binary>>}
       end);
unparse_questions(Questions) -> unparse_questions(Questions, <<>>).

unparse_questions([], RawQuestions) -> {raw_questions, RawQuestions};
unparse_questions([#question{qname=QName, qtype=QType, qclass=QClass}|Questions], RawQuestions) ->
    ?INVALIDCATCH(
       question,
       begin
           {raw_qname, RawQName} = unparse_domain(QName),
           {raw_qtype, RawQType} = unparse_qtype(QType),
           {raw_qclass, RawQClass} = unparse_qclass(QClass),
           unparse_questions(<<RawQuestions/binary,
                              RawQName/binary,
                              RawQType/binary,
                              RawQClass/binary>>, Questions)
       end).

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
    ?INVALIDCATCH(
       raw_resource_record,
       begin
           {domain,
            Name,
            <<RawType:16, RawClass:16, TTL:32,
             RDLength:16, RawRData:RDLength/binary, Rest/binary>>} = parse_domain(Body),
           {type, Type} = parse_type(RawType),
           {class, Class} = parse_class(RawClass),
           {rdata, RData} = parse_rdata(Type, RawRData),
           RR = #resource_record{name = Name, type = Type, class = Class,
                                 ttl = TTL, rdata = RData},
           parse_resource_records(Count - 1, Rest, [RR|RRs])
       end).

%% @doc Unparse resource records.
%% @private Internal helper function.
%% @since 0.2.0
unparse_resource_record({resource_records, RRs, Rest}) ->
    ?INVALIDCATCH(
       resource_record,
        begin
            {raw_resource_records, RawRRs} = unparse_resource_record(RRs),
            {raw_resource_records, <<RawRRs/binary, Rest/binary>>}
        end);
unparse_resource_record(RRs) -> unparse_resource_record(<<>>, RRs).

unparse_resource_record(RawRRs, []) -> RawRRs;
unparse_resource_record(RawRRs, [#resource_record{name=Name, type=Type, class=Class, ttl=TTL, rdata=RData}|RRs]) ->
    ?INVALIDCATCH(
       resource_record,
       begin
           {raw_domain, RawName} = unparse_domain(Name),
           {raw_type, RawType} = unparse_type(Type),
           {raw_class, RawClass} = unparse_class(Class),
           {raw_rdata, RawRData} = unparse_rdata(Type, RData),
           {raw_rdlength, RawRDLength} = length(RawRData),
           {raw_resource_record, unparse_resource_record(<<RawRRs/binary,
                                                          RawName/binary,
                                                          RawType/binary,
                                                          RawClass/binary,
                                                          TTL:32,
                                                          RawRDLength:16,
                                                          RawRData/binary>>, RRs)}
       end).

%% @doc Parse RDATA, the data of a resource record.
%% @private Internal helper function.
%% @since 0.2.0
parse_rdata(a,     _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(ns,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(md,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(mf,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(cname, RawRData) ->
    ?INVALIDCATCH(
       raw_rdata,
       begin
           {domain, Domain, _Rest} = parse_domain(RawRData),
           {rdata, Domain}
       end);
parse_rdata(soa,   _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(mb,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(mg,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(mr,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(null,  _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(wks,   _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(ptr,   _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(hinfo, _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(minfo, _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(mx,    _RawRData) -> {error, [not_implemented_yet]};
parse_rdata(_Type, _RawRData) ->
    {invalid, [raw_rdata]}.

%% @doc Unparse RDATA, the data of a resource record.
%% @private Internal helper function.
%% @since 0.2.0
unparse_rdata(a,     _RData) -> {error, [not_implemented_yet]};
unparse_rdata(ns,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(md,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(mf,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(cname, RData) ->
    ?INVALIDCATCH(
       raw_rdata,
       begin
           {raw_domain, RawDomain, _Rest} = unparse_domain(RData),
           {rdata, RawDomain}
       end);
unparse_rdata(soa,   _RData) -> {error, [not_implemented_yet]};
unparse_rdata(mb,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(mg,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(mr,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(null,  _RData) -> {error, [not_implemented_yet]};
unparse_rdata(wks,   _RData) -> {error, [not_implemented_yet]};
unparse_rdata(ptr,   _RData) -> {error, [not_implemented_yet]};
unparse_rdata(hinfo, _RData) -> {error, [not_implemented_yet]};
unparse_rdata(minfo, _RData) -> {error, [not_implemented_yet]};
unparse_rdata(mx,    _RData) -> {error, [not_implemented_yet]};
unparse_rdata(_Type, _RData) ->
    {invalid, [rdata]}.

%% @doc Parse a DNS domain.
%% @private Internal helper function.
%% @since 0.2.0
parse_domain(Body) -> parse_domain([], Body).
parse_domain(Labels, <<Length:8, Label:Length/binary-unit:8, Rest/binary>>) when Length > 0 ->
    parse_domain([binary_to_list(Label)|Labels], Rest);
parse_domain(Labels, <<Length:8, Rest/binary>>) when Length == 0 ->
    {domain, lists:reverse(Labels), Rest};
parse_domain(_Labels, _Body) ->
    {invalid, [domain]}.

%% @doc Unparse a DNS domain.
%% @private Internal helper function.
%% @since 0.2.0
unparse_domain({domain, Domain, Rest}) ->            %% TODO: Is this instance really needed ?
    {raw_domain, RawDomain} = unparse_domain(Domain),
    {raw_domain, <<RawDomain/binary, Rest/binary>>};
unparse_domain(Domain) -> {raw_domain, unparse_domain(<<>>, Domain)}.

unparse_domain(RawDomain, []) -> <<RawDomain/binary, 0:8>>;  %% TODO: wouldn't it make sense to swap the arguments here ?
unparse_domain(RawDomain, [Label|Labels]) ->
    LabelLength = length(Label),
    BinaryLabel = list_to_binary(Label),
    unparse_domain(<<RawDomain/binary, LabelLength:8, BinaryLabel/binary>>, Labels).

%% @doc Turn a numeric DNS type into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_type(<< 1:16>>) -> {type, a};
parse_type(<< 2:16>>) -> {type, ns};
parse_type(<< 3:16>>) -> {type, md};
parse_type(<< 4:16>>) -> {type, mf};
parse_type(<< 5:16>>) -> {type, cname};
parse_type(<< 6:16>>) -> {type, soa};
parse_type(<< 7:16>>) -> {type, mb};
parse_type(<< 8:16>>) -> {type, mg};
parse_type(<< 9:16>>) -> {type, mr};
parse_type(<<10:16>>) -> {type, null};
parse_type(<<11:16>>) -> {type, wks};
parse_type(<<12:16>>) -> {type, ptr};
parse_type(<<13:16>>) -> {type, hinfo};
parse_type(<<14:16>>) -> {type, minfo};
parse_type(<<15:16>>) -> {type, mx};
parse_type(<<16:16>>) -> {type, txt};
parse_type(_) ->         {invalid, [raw_type]}.

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
unparse_type(_) ->     {invalid, [type]}.

%% @doc Turn a numeric DNS qtype into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_qtype(<<252:16>>) -> {qtype, axfr};
parse_qtype(<<253:16>>) -> {qtype, mailb};
parse_qtype(<<254:16>>) -> {qtype, maila};
parse_qtype(<<255:16>>) -> {qtype, all};
parse_qtype(RawQType) ->
    case parse_type(RawQType) of
        {type, QType} -> {qtype, QType};
        {invalid, [raw_type]} -> {invalid, [raw_qtype]}
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
        {invalid, [type]} -> {invalid, [qtype]}
    end.

%% @doc Turn a numeric DNS class into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_class(<<1:16>>) -> {class, in};
parse_class(<<2:16>>) -> {class, cs};
parse_class(<<3:16>>) -> {class, ch};
parse_class(<<4:16>>) -> {class, hs};
parse_class(_) ->        {invalid, [raw_class]}.

%% @doc Unparse a DNS class.
%% @private Internal helper function.
%% @since 0.2.0
unparse_class(in) -> {raw_class, <<1:16>>};
unparse_class(cs) -> {raw_class, <<2:16>>};
unparse_class(ch) -> {raw_class, <<3:16>>};
unparse_class(hs) -> {raw_class, <<4:16>>};
unparse_class(_)  -> {invalid, [class]}.

%% @doc Turn a numeric DNS qclass into an atom.
%% @private Internal helper function.
%% @since 0.2.0
parse_qclass(<<255:16>>) -> {qclass, any};
parse_qclass(RawClass) ->
    case parse_class(RawClass) of
        {class, Class} -> {qclass, Class};
        {invalid, [raw_class]} -> {invalid, [raw_qclass]}
    end.

%% @doc Unparse a DNS qclass.
%% @private Internal helper function.
%% @since 0.2.0
unparse_qclass(any) -> {raw_qclass, <<255:16>>};
unparse_qclass(Class) ->
    case unparse_class(Class) of
        {raw_class, RawClass} -> {raw_qclass, RawClass};
        {invalid, [class]} -> {invalid, [qclass]}
    end.

%% @doc Parse a DNS qr.
%% @private Internal helper function.
%% @since 0.2.0
parse_qr(0) -> {qr, query_};
parse_qr(1) -> {qr, response};
parse_qr(_) -> {invalid, [raw_qr]}.

%% @doc Parse a DNS qr.
%% @private Internal helper function.
%% @since 0.2.0
unparse_qr(query_)   -> {raw_qr, 0};
unparse_qr(response) -> {raw_qr, 1};
unparse_qr(_) ->        {invalid, [qr]}.

%% @doc Parse an opcode.
%% @private Internal helper function.
%% @since 0.2.0
parse_opcode(0) -> {opcode, query_};
parse_opcode(1) -> {opcode, iquery};
parse_opcode(2) -> {opcode, status};
parse_opcode(_) -> {invalid, [raw_opcode]}.

%% @doc Unpparse an opcode.
%% @private Internal helper function.
%% @since 0.2.0
unparse_opcode(query_) -> {raw_opcode, 0};
unparse_opcode(iquery) -> {raw_opcode, 1};
unparse_opcode(status) -> {raw_opcode, 2};
unparse_opcode(_) ->      {invalid, [opcode]}.

%% @doc Parse DNS RCodes.
%% @private Internal helper function.
%% @since 0.2.0
parse_rcode(0) -> {rcode, no_error};
parse_rcode(1) -> {rcode, format_error};
parse_rcode(2) -> {rcode, server_failure};
parse_rcode(3) -> {rcode, name_error};
parse_rcode(4) -> {rcode, not_implemented};
parse_rcode(5) -> {rcode, refused};
parse_rcode(_) -> {invalid, [raw_rcode]}.

%% @doc Unparse DNS RCodes.
%% @private Internal helper function.
%% @since 0.2.0
unparse_rcode(no_error) ->        {raw_rcode, 0};
unparse_rcode(format_error) ->    {raw_rcode, 1};
unparse_rcode(server_failure) ->  {raw_rcode, 2};
unparse_rcode(name_error) ->      {raw_rcode, 3};
unparse_rcode(not_implemented) -> {raw_rcode, 4};
unparse_rcode(refused) ->         {raw_rcode, 5};
unparse_rcode(_) ->               {invalid, [rcode]}.

%% @doc Parse boolean values.
%% @private Internal helper function.
%% @since 0.2.0
parse_bool(0) -> {bool, false};
parse_bool(1) -> {bool, true};
parse_bool(_) -> {invalid, [raw_bool]}.

%% @doc Unparse boolean values.
%% @private Internal helper function.
%% @since 0.2.0
unparse_bool(false) -> {raw_bool, 0};
unparse_bool(true) ->  {raw_bool, 1};
unparse_bool(_) ->     {invalid, [bool]}.

%% @doc true if the argument is an error, non-true otherwise (false).
%% @private Internal helper function.
%% @since 0.2.0
is_invalid({invalid, _}) -> true;
is_invalid(_) ->          false.

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
  		{correct, txt,   <<16:16>>},
                {error,   err,   <<17:16>>}]).

%% DNS QTypes to test the parser.
-define(QTYPES, [{error,   err,   <<128:16>>},
                 {correct, axfr,  <<252:16>>},
 		 {correct, mailb, <<253:16>>},
 		 {correct, maila, <<254:16>>},
  		 {correct, all,   <<255:16>>}] ++ ?TYPES).

%% DNS Classes to test the parser.
-define(CLASSES, [{correct, in, <<1:16>>},
 		  {correct, cs, <<2:16>>},
 		  {correct, ch, <<3:16>>},
  		  {correct, hs, <<4:16>>},
                  {error,   wh, <<9:16>>}]).

%% DNS QClasses to test the parser.
-define(QCLASSES, [{correct, any, <<255:16>>},
                   {error,   err, <<254:16>>}] ++ ?CLASSES).

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

    DomainParsingTests ++ DomainUnparsingTests ++
        QuestionsParsingTests.%%, QuestionsUnparsingTests.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%% DNS Message Parsing testing %%%%%%%%%%%

%% %% @doc .
%% %% @private Internal helper function.
%% %% @since 0.2.0
%% %%build_messages(_Questions, _RRs) ->
%% %%  [].

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
             io_lib:format("~nType: ~p~nCParsed: ~p~nCount: ~p~nCRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Count, CRaw, ParsedToTest])),
    if Type == correct -> [{Desc, ?_assert(ParsedToTest == CParsed)} |  questions_parsing_tests(Questions)];
       true -> questions_parsing_tests(Questions)
    end.
%%    [{Desc,
%%     [{Desc, case Type of
%%                 correct ->
%%                     ?_assert(ParsedToTest == CParsed);
%%                 error ->
%%                     ?_assert((is_invalid(ParsedToTest)) or % We should get an error
%%                              (ParsedToTest /= CParsed))  % or plain wrong data (not an exception).
%%             end} | questions_parsing_tests(Questions)].

%% @doc Having a list of Questions build the unparsing tests.
%% @private Internal helper function.
%% @todo tail-optimize.
%% @since 0.2.0
questions_unparsing_tests([]) -> [];
questions_unparsing_tests([{Type, _Count, Parsed, Raw}|Questions]) ->
    Noise = list_to_binary(noise()),
    CRaw = <<Raw/binary, Noise/binary>>,              % Complete raw, add noise.
    CParsed = {questions, Parsed, Noise},             % Complete parsed, add signature and noise.
    RawToTest = (catch unparse_questions(CParsed)),   % Perform the unparsing.
    Desc = lists:flatten(                             % Some useful description
             io_lib:format("~nType: ~p~nCParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, CParsed, CRaw, RawToTest])),
    [{Desc, case Type of                              % What kind of test is it ?
                correct ->
                    ?_assert(RawToTest == CRaw);
                error   ->
                    ?_assert((is_invalid(RawToTest)) or % We should get an error
                             (RawToTest /= CRaw))     % or plain wrong data (not an exception).
            end} | questions_unparsing_tests(Questions)].

%% @doc Using domains Domains, QTypes and QClasses build all possible questions up to length Length (that is, chained questions). 
%% @private Internal helper function.
%% @since 0.2.0
build_questions(Domains, QTypes, QClasses, Length, Sample) ->
    Questions = n_of(Sample, build_questions(Domains, QTypes, QClasses, Sample)),
    BuildQuestions = fun(N, NewQuestions) -> % Function to build questions of N length and append it to NewQuestions.
                             NewQuestions ++ build_questions(Questions, N, Sample)
                     end,
    n_of(Sample, lists:foldl(BuildQuestions, [], lists:seq(1, Length))).

%% @doc Make all the possible combinations for a set of Domains, QTypes and QClasses. 
%% @private Internal helper function.
%% @since 0.2.0
build_questions([], _QTypes, _QClasses, _Sample) -> [];
build_questions(_Domains, [], _QClasses, _Sample) -> [];
build_questions(_Domains, _QTypes, [], _Sample) -> [];
build_questions({DomainType, DomainParsed, DomainRaw},
                {QTypeType,  QTypeParsed,  QTypeRaw},
                {QClassType, QClassParsed, QClassRaw}, _Sample) ->
    Type = if (DomainType == correct) and (QTypeType == correct) and (QClassType == correct) -> correct;
              true -> error
           end,
    {Type, 1,
     [#question{qname = DomainParsed, qtype = QTypeParsed, qclass = QClassParsed}],
     <<DomainRaw/binary, QTypeRaw/binary, QClassRaw/binary>>};
build_questions({DomainType, DomainParsed, DomainRaw},
                {QTypeType,  QTypeParsed,  QTypeRaw},
                QClasses, Sample) ->
    BuildQuestion = fun(QClass) ->
                            Sample, build_questions({DomainType, DomainParsed, DomainRaw},
                                                    {QTypeType, QTypeParsed, QTypeRaw},
                                                    QClass, Sample)
                    end,
    n_of(Sample, lists:flatten(lists:map(BuildQuestion, QClasses)));
build_questions({DomainType, DomainParsed, DomainRaw}, QTypes, QClasses, Sample) ->
    BuildQuestion = fun(QType) ->
                            n_of(Sample, build_questions({DomainType, DomainParsed, DomainRaw},
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource Record Parsing and Unparsing Testing %%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% RData Parsing and Unparsing testing %%%%%%

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
             io_lib:format("~nType: ~p~nCParsed: ~p~nCRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, CRaw, ParsedToTest])),
    [{Desc, case Type of                         % What kind of test is it ?
                correct ->
                    ?_assert(ParsedToTest == CParsed);
                error   ->
                    ?_assert((is_invalid(ParsedToTest)) or % We should get an error
                             (ParsedToTest /= CParsed))  % or plain wrong data (not an exception).
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
             io_lib:format("~nType: ~p~nCParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, CParsed, CRaw, RawToTest])),
    [{Desc, case Type of                         % What kind of test is it ?
                correct ->
                    ?_assert(RawToTest == CRaw);
                error   ->
                    ?_assert((is_invalid(RawToTest)) or % We should get an error
                             (RawToTest /= CRaw))     % or plain wrong data (not an exception).
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
%%%%%% Type Parsing and Unparsing testing %%%%%%

type_parsing_test_() -> type_parsing_tests(?TYPES).
type_parsing_tests([]) -> [];
type_parsing_tests([{Type, Parsed, Raw}|Types]) ->
    ParsedToTest = parse_type(Raw),                         % Perform the parsing.
    CParsed = {type, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | type_parsing_tests(Types)].

type_unparsing_test_() -> type_unparsing_tests(?TYPES).
type_unparsing_tests([]) -> [];
type_unparsing_tests([{Type, Parsed, Raw}|Types]) ->
    RawToTest = unparse_type(Parsed),                    % Perform the parsing.
    CRaw = {raw_type, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | type_unparsing_tests(Types)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% QType Parsing and Unparsing testing %%%%%%

qtype_parsing_test_() -> qtype_parsing_tests(?QTYPES).
qtype_parsing_tests([]) -> [];
qtype_parsing_tests([{Type, Parsed, Raw}|QTypes]) ->
    ParsedToTest = parse_qtype(Raw),                        % Perform the parsing.
    CParsed = {qtype, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | qtype_parsing_tests(QTypes)].

qtype_unparsing_test_() -> qtype_unparsing_tests(?QTYPES).
qtype_unparsing_tests([]) -> [];
qtype_unparsing_tests([{Type, Parsed, Raw}|QTypes]) ->
    RawToTest = unparse_qtype(Parsed),                   % Perform the parsing.
    CRaw = {raw_qtype, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | qtype_unparsing_tests(QTypes)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Class Parsing and Unparsing testing %%%%%%

class_parsing_test_() -> class_parsing_tests(?CLASSES).
class_parsing_tests([]) -> [];
class_parsing_tests([{Type, Parsed, Raw}|Classes]) ->
    ParsedToTest = parse_class(Raw),                        % Perform the parsing.
    CParsed = {class, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | class_parsing_tests(Classes)].

class_unparsing_test_() -> class_unparsing_tests(?CLASSES).
class_unparsing_tests([]) -> [];
class_unparsing_tests([{Type, Parsed, Raw}|Classes]) ->
    RawToTest = unparse_class(Parsed),                   % Perform the parsing.
    CRaw = {raw_class, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | class_unparsing_tests(Classes)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% QClass Parsing and Unparsing testing %%%%%%

qclass_parsing_test_() -> qclass_parsing_tests(?QCLASSES).
qclass_parsing_tests([]) -> [];
qclass_parsing_tests([{Type, Parsed, Raw}|QClasses]) ->
    ParsedToTest = parse_qclass(Raw),                       % Perform the parsing.
    CParsed = {qclass, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | qclass_parsing_tests(QClasses)].

qclass_unparsing_test_() -> qclass_unparsing_tests(?QCLASSES).
qclass_unparsing_tests([]) -> [];
qclass_unparsing_tests([{Type, Parsed, Raw}|QClasses]) ->
    RawToTest = unparse_qclass(Parsed),                  % Perform the parsing.
    CRaw = {raw_qclass, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | qclass_unparsing_tests(QClasses)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Qr Parsing and Unparsing testing %%%%%%

qr_parsing_test_() -> qr_parsing_tests(?QRS).
qr_parsing_tests([]) -> [];
qr_parsing_tests([{Type, Parsed, Raw}|Qrs]) ->
    ParsedToTest = parse_qr(Raw),                           % Perform the parsing.
    CParsed = {qr, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | qr_parsing_tests(Qrs)].

qr_unparsing_test_() -> qr_unparsing_tests(?QRS).
qr_unparsing_tests([]) -> [];
qr_unparsing_tests([{Type, Parsed, Raw}|Qrs]) ->
    RawToTest = unparse_qr(Parsed),                      % Perform the parsing.
    CRaw = {raw_qr, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | qr_unparsing_tests(Qrs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% OpCode Parsing and Unparsing testing %%%%%%

opcode_parsing_test_() -> opcode_parsing_tests(?OPCODES).
opcode_parsing_tests([]) -> [];
opcode_parsing_tests([{Type, Parsed, Raw}|OpCodes]) ->
    ParsedToTest = parse_opcode(Raw),                       % Perform the parsing.
    CParsed = {opcode, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | opcode_parsing_tests(OpCodes)].

opcode_unparsing_test_() -> opcode_unparsing_tests(?OPCODES).
opcode_unparsing_tests([]) -> [];
opcode_unparsing_tests([{Type, Parsed, Raw}|OpCodes]) ->
    RawToTest = unparse_opcode(Parsed),                  % Perform the parsing.
    CRaw = {raw_opcode, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | opcode_unparsing_tests(OpCodes)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% RCode Parsing and Unparsing testing %%%%%%

rcode_parsing_test_() -> rcode_parsing_tests(?RCODES).
rcode_parsing_tests([]) -> [];
rcode_parsing_tests([{Type, Parsed, Raw}|RCodes]) ->
    ParsedToTest = parse_rcode(Raw),                        % Perform the parsing.
    CParsed = {rcode, Parsed},
    Desc = lists:flatten(                                   % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                    % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | rcode_parsing_tests(RCodes)].

rcode_unparsing_test_() -> rcode_unparsing_tests(?RCODES).
rcode_unparsing_tests([]) -> [];
rcode_unparsing_tests([{Type, Parsed, Raw}|RCodes]) ->
    RawToTest = unparse_rcode(Parsed),                   % Perform the parsing.
    CRaw = {raw_rcode, Raw},
    Desc = lists:flatten(                                % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                 % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | rcode_unparsing_tests(RCodes)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Bool Parsing and Unparsing testing %%%%%%

bool_parsing_test_() -> bool_parsing_tests(?BOOLEANS).
bool_parsing_tests([]) -> [];
bool_parsing_tests([{Type, Parsed, Raw}|Bools]) ->
    ParsedToTest = parse_bool(Raw),  % Perform the parsing.
    CParsed = {bool, Parsed},
    Desc = lists:flatten(            % Some useful description.
             io_lib:format("~nType: ~p~nCParsed: ~p~nRaw: ~p~nParsedToTest: ~p~n", [Type, CParsed, Raw, ParsedToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(ParsedToTest == CParsed);
                error   -> ?_assert(is_invalid(ParsedToTest)) % We should get an error.
            end} | bool_parsing_tests(Bools)].

bool_unparsing_test_() -> bool_unparsing_tests(?BOOLEANS).
bool_unparsing_tests([]) -> [];
bool_unparsing_tests([{Type, Parsed, Raw}|Bools]) ->
    RawToTest = unparse_bool(Parsed), % Perform the parsing.
    CRaw = {raw_bool, Raw},
    Desc = lists:flatten(             % Some useful description.
             io_lib:format("~nType: ~p~nParsed: ~p~nCRaw: ~p~nRawToTest: ~p~n", [Type, Parsed, CRaw, RawToTest])),
    [{Desc, case Type of                                                % What kind of test is it ?
                correct -> ?_assert(RawToTest == CRaw);
                error   -> ?_assert(is_invalid(RawToTest)) % We should get an error.
            end} | bool_unparsing_tests(Bools)].

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

-endif. %% ifdef(TEST).
