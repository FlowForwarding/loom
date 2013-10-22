% vim: ts=2:sw=2:noet:tw=80
% dnspkt.erl: Parse a dns query packet.

% Copyright 2013, Olof Johansson <olof@ethup.se>
%
% Copying and distribution of this file, with or without
% modification, are permitted in any medium without royalty
% provided the copyright notice are preserved. This file is
% offered as-is, without any warranty.

-module(dnspkt).
-export([parse_pkt/1]).

-include_lib("eunit/include/eunit.hrl").

-record(dns_header, {
	id,
	qr, opcode, aa, tc, rd, ra, z, rcode,
	qdcount,
	ancount,
	nscount,
	arcount
}).

-record(dns_question, {
	name, type, class
}).

-record(dns_rr, {
	name, type, class, ttl, rdlength, rdata
}).

% Parse a dns query packet and return a structure of the
% following format:
%
% {
%  header, #dns_header,
%  question, [#dns_question, ...],
%  answer, {#dns_rr, ...],
%  auth, {#dns_rr, ...],
%  additional, {#dns_rr, ...],
% }
%
% dns_question is a record with the fields:
%   name, type and class.
%
% dns_rr is a record with the fields:
%   name, type, class, ttl, rdlength, rdata
%
% name is a list strings, each corresponding to the labels of
% the domain name. You could join the on . to get a human
% readable representation.
%
% type, class, ttl, rdlength are integers.
%
% rdata is arbitrary data.
%
% Recommended reading:
%   RFC 1034 - DNS architecture
%   RFC 1035 - DNS implementation
%
parse_pkt(RawPkt) ->
	{Header, HeaderTail} = parse_header(RawPkt),

	{Question, QuestionTail} = parse_qsection(
		HeaderTail, Header#dns_header.qdcount
	),
	{Answer, AnswerTail} = parse_section(
		QuestionTail, Header#dns_header.ancount
	),
	{Auth, AuthTail} = parse_section(
		AnswerTail, Header#dns_header.nscount
	),
	{Additional, _AdditionalTail} = parse_section(
		AuthTail, Header#dns_header.arcount
	),

	{
		{ header, Header },
		{ question, Question },
		{ answer, Answer },
		{ auth, Auth },
		{ additional, Additional }
	}.

% Extracts the first three bytes, and extracting the semantic
% meaning of the bits; returns a tuple with a #dns_header record
% and the rest of the packet, following the header.
parse_header(RawPkt) ->
	<<
		Id:16,
		Qr:1, Opcode:4, Aa:1, Tc:1, Rd:1, Ra:1, Z:3, Rcode:4,
		Qdcount:16,
		Ancount:16,
		Nscount:16, Arcount:16,
		Tail/binary
	>> = RawPkt,

	{#dns_header{
		id=Id,
		qr=Qr, opcode=Opcode, aa=Aa, tc=Tc, rd=Rd, ra=Ra, z=Z, rcode=Rcode,
		qdcount=Qdcount,
		ancount=Ancount,
		nscount=Nscount,
		arcount=Arcount
	}, Tail}.

% Returns a tuple of {Entries, RestOfDnsPkt} where Entries
% is a list of #dns_questions, and RestOfDnsPkt is the part
% of the dns packet after the question section.
parse_qsection(RawPkt, Count) ->
	parse_qsection(RawPkt, Count, []).

parse_qsection(Tail, 0, Entries) ->
	{Entries, Tail};
parse_qsection(RawPkt, Count, Entries) ->
	{Entry, Tail} = parse_qentry(RawPkt),
	parse_qsection(Tail, Count-1, Entries ++ [Entry]).

parse_qentry(RawPkt) ->
	{Qname, QnameTail} = parse_qname(RawPkt),
	<<Qtype:16, Qclass:16, Tail/binary>> = QnameTail,
	{#dns_question{name=Qname, type=Qtype, class=Qclass}, Tail}.

parse_section(_RawPkt, 0) ->
	[];
parse_section(_RawPkt, _Count) ->
	[].

parse_qname(RawPkt) ->
	parse_qname(RawPkt, []).

parse_qname(RawPkt, Labels) ->
	case get_label(RawPkt) of
		{<<"">>, Tail} -> {Labels, Tail};
		{L, Tail} ->
			parse_qname(Tail, Labels ++ [L])
	end.

% Returns a tuple consisting of a domain name label (e.g. www in
% www.example.com) and the rest of the dns packet.
get_label(RawPkt) ->
	<<Len:8, LenTail/binary>> = RawPkt,

	case Len of
		0 ->
			Label = <<"">>,
			LabelTail = LenTail;
		_ ->
			<<Label:Len/binary, LabelTail/binary>> = LenTail
	end,

	{Label, LabelTail}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNIT TESTS, motherfucker do you speak it                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_header_tests_() ->
	[
		?_assert(parse_header(
			<<
				32768:16,
				1:1, 0:4, 0:1, 0:1, 1:1, 0:1, 0:3, 0:4,
				1:16,
				0:16,
				0:16,
				0:16,
				"random data"
			>>) =:= {
				#dns_header{
					id=32768,
					qr=1,
					opcode=0,
					aa=0,
					tc=0,
					rd=1,
					ra=0,
					z=0,
					rcode=0,
					qdcount=1,
					ancount=0,
					nscount=0,
					arcount=0
				}, <<"random data">>
			}
		)
	].

get_label_test_() ->
	[
		?_assert(
			get_label(<<7:8, "example", 3:8, "com", 0:8, "tail">>)
			=:=
			{<<"example">>, <<3:8, "com", 0:8, "tail">>}
		),
		?_assert(
			get_label(<<3:8, "com", 0:8>>)
			=:=
			{<<"com">>, <<0:8>>}
		),
		?_assert(
			get_label(<<0:8, "tail">>)
			=:=
			{<<"">>, <<"tail">>}
		),
		?_assert(
			get_label(<<0:8>>)
			=:=
			{<<"">>, <<"">>}
		)
	].

parse_qname_test_() ->
	[
		?_assert(
			parse_qname(<<7:8, "example", 3:8, "com", 0:8, "tail">>)
			=:=
			{[<<"example">>, <<"com">>], <<"tail">>}
		),
		?_assert(
			parse_qname(<<7:8, "example", 3:8, "com", 0:8>>)
			=:=
			{[<<"example">>, <<"com">>], <<"">>}
		),
		?_assert(
			parse_qname(<<0:8, "tail">>)
			=:=
			{[], <<"tail">>}
		),
		?_assert(
			parse_qname(<<0:8>>)
			=:=
			{[], <<"">>}
		)
	].

parse_qsection_test_() ->
	[
		?_assert(
			parse_qsection(<<7:8, "example", 3:8, "com", 0:8, 6:16, 1:16, "tail">>, 1)
			=:=
			{
				[
					#dns_question{
						name=[<<"example">>, <<"com">>],
						type=6,
						class=1
					}
				], <<"tail">>
			}
		),
		?_assert(
			parse_qsection(<<7:8, "example", 3:8, "com", 0:8, 6:16, 1:16>>, 1)
			=:=
			{
				[
					#dns_question{
						name=[<<"example">>, <<"com">>],
						type=6,
						class=1
					}
				], <<"">>
			}
		),
		?_assert(
			parse_qsection(<<0:8, 1:16, 1:16>>, 1)
			=:=
			{
				[
					#dns_question{
						name=[],
						type=1,
						class=1
					}
				], <<"">>
			}
		),

		% Multiple question entries in qsection
		?_assert(
			parse_qsection(
				<<
					0:8, 1:16, 1:16,
					7:8, "example", 3:8, "com", 0:8, 1:16, 1:16,
					3:8, "iis", 2:8, "se", 0:8, 1:16, 1:16
				>>, 3
			)
			=:=
			{
				[
					#dns_question{
						name=[],
						type=1,
						class=1
					},
					#dns_question{
						name=[<<"example">>, <<"com">>],
						type=1,
						class=1
					},
					#dns_question{
						name=[<<"iis">>, <<"se">>],
						type=1,
						class=1
					}
				], <<"">>
			}
		)
	].
