-module(krpc).
-export([
	query/2, query/3, find_node/2
]).

-define(TXID_LEN, 64).

query(NodeId, QType) ->
	query(NodeId, QType, {}).
query(NodeId, QType, Args) ->
	send_request(NodeId, gen_query(QType, Args)).

gen_query(QType, Args) -> [
		{$t, $q},
		{$q, QType},
		{$a, [ {$t, gen_txid()} | Args ]}
	].

send_request(_Node, Req) ->
	_Raw = bencode:bencode([{$t, gen_txid()}|Req]).
	% TODO
	%  - Lookup _Node's IP network address
	%  - Send the blob

find_node(Dest, Target) -> query(Dest, find_peer, {target, Target}).

%% gen_txid(): Generate transaction ID for use in krpc queries
gen_txid() ->
	gen_txid(0, fun(Len, _Seed) ->
		crypto:bytes_to_integer(
			crypto:strong_rand_bytes(Len div 8)
		)
	end).
%-ifdef(TEST).
%% gen_txid(Seed): Weaker randomness for testing
%gen_txid(Seed) ->
%	gen_txid(Seed, fun(Len, InSeed) ->
%		random:uniform( 1 bsl (Len-1), InSeed )
%	end).
%-endif.
gen_txid(Seed, RandFun) -> RandFun(?TXID_LEN, Seed).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

txid_test() ->
	?assertNotEqual(gen_txid(), gen_txid()).

-endif.
