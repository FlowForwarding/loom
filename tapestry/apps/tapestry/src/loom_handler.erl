
-module(loom_handler).

-export([handle_message/2]).

-include("tap_logger.hrl").
-include_lib("pkt/include/pkt.hrl").


%%% Handling packet_in message
%%-spec handle_message(ofp_message(), ofs_state()) -> ok.
handle_message({packet_in, _Xid, Body}, _State) ->
    % received a message on the connection
    Reason = proplists:get_value(reason, Body),    
    TableId = proplists:get_value(table_id, Body),    
    Match = proplists:get_value(match, Body),
    Data = proplists:get_value(data, Body),        
    process_packetin(Reason, TableId, Match, Data),
    ok.

%packetin
process_packetin(action, _TableId, _Match, Data) ->
    dns_reply(Data);
process_packetin(nomatch, _TableId, _Match, _Data) ->
    ?DEBUG("packetin reason = nomatch~n");
process_packetin(Reason, _TableId, _Match, _Data) ->
    ?DEBUG("packetin reason = ~p~n", [Reason]).

dns_reply(Data)->
    try
	Packet = pkt:decapsulate({ether,Data}),
	case Packet of 
	    [EthHeader, Header1, Header2, Payload] ->
		[ether|_] = tuple_to_list(EthHeader),
		[Type1|_] = tuple_to_list(Header1),
		[Type2|_] = tuple_to_list(Header2),
		Result = case (Type1 == ipv4) and (Type2 == udp) of
			     true ->  
				 inet_dns:decode(Payload);
			     _ -> unknown
			 end,
		case Result of
		    {ok, DnsRec} -> 
			Match = match_reply(DnsRec),
			case Match of
			    {error, _} ->
                                ?DEBUG("No match dropped: ~p~n",[Match]);
			    ID ->
				R = list_to_tuple(
                                        binary_to_list(Header1#ipv4.daddr)),
				SendValue = {R, ID},
				?DEBUG("Sending: ~p~n",[SendValue]),
                                tap_aggr:dns_rpely(SendValue)
			end;
		    _ -> ?DEBUG("No match dropped: ~p~n",[Result])
		end;
	    _ -> ?DEBUG("No match dropped: ~p~n",[Packet])
	end
    catch
	Error ->
	    ?DEBUG("Decapsulation Error:~p Data: ~p~n",[Error, Data])
    end.

match_reply({dns_rec, {dns_header, _, true, _, _, _, _, _, _, _},
                      [{dns_query, _, a, in}], RRSet, _, _})->
    Record = lists:keyfind(a, 3, RRSet),
    case Record of
	false ->
	    {error, no_a_record};
	{dns_rr, _, a, _, _, _, ID, _, _, _} -> ID
    end;
match_reply(_) ->
    {error, bad_response}.
