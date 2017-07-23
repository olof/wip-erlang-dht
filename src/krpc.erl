-module(krpc).
-export([
	init/1, query/3, find_node/2
]).
-include("kademlia_state.hrl").

%% TXID_LEN is the length of the transaction ID in bits.
-define(TXID_LEN, 64).

query(State, To, QType, Args, Callback) ->
	send_request(State, To, gen_query(QType, Args), Callback).

%% generate a query of type QType, with args Args, suitable for
%% sending over the network.
gen_query(QType, Args) -> [ {$y, $q}, {$q, QType}, {$a, Args} ].

send_request(State, Node, Req, Callback) ->
	EncReq = bencode:encode([{$t, gen_txid()}|Req]),
	IPaddr = find_node(State, Node),
	Sock = gen_udp:open(Node[2]),
	% TODO
	%  - Lookup _Node's IP network address
	%  - Send the blob

%% ping another kademlia node
%%    id: the id of the sender (me)
ping(State, Node, Callback) ->
	query(State, Node, "ping", [{"id", State#node_id}], Callback).

%% find other nodes in the kademlia network
find_node(State, NodeId) ->
	case lookup_cache(NodeId) of
		{ok, NodeContact} -> NodeContact;
		notfound -> gen_query("find_node",
		            	[{"target", NodeId}, {"id", Me}]
		            )
	end,
	gen_query("find_node", [{}]).

get_peers() -> not_implemented.
announce_peer() -> not_implemented.

find_node(Me, Target) ->
	query(Target, find_node, [
		{t, Target},
		{id, Me}
	]).

%% gen_txid(): Generate transaction ID for use in krpc queries
gen_txid() ->
	crypto:bytes_to_integer(crypto:strong_rand_bytes(?TXID_LEN div 8))

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

txid_test() ->
	?assertNotEqual(gen_txid(), gen_txid()).

-endif.
