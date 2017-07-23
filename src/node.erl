-module(node).
-export([id/0, distance/2]).

%% TODO: Generate node id (not this static fake)
id() -> "98d832967f9dacbf60381657a6cd5e077cb8d3cb".
%% TODO: OTP behavior
%% TODO: joining network

%% Calculate the Kademlia distance between two nodes: The XOR of
%% two node IDs.  The IDs should be provided as a hexadecimal
%% representation.
%%
%% FIXME: note that there currently is no validation that the IDs
%%        are the same length (and thus, potentially within the
%%        same namespace), that is currently up to the caller to
%%        verify.
distance(Node1, Node2) -> id_int(Node1) bxor id_int(Node2).

%% id_{hex,int}, converts to/from integer/hexadecimal string representation
id_hex(Int) when is_integer(Int) -> integer_to_list(Int, 16);
id_hex(HexStr) -> HexStr.
id_int(HexStr) when is_list(HexStr) -> list_to_integer(HexStr, 16);
id_int(Int) -> Int.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

min_sha1() -> string:chars($0, 40).
max_sha1() -> string:chars($F, 40).

distance_test() ->
	lists:foreach(
		fun({Res, {Id1, Id2}}) ->
			?assertEqual(Res, distance( Id1, Id2 ))
		end, [
			{0, {"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
			     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}},
			{1, {"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
			     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"}},
			{id_int(max_sha1()), {min_sha1(), max_sha1()}},
			{id_int(max_sha1())-1, {min_sha1()+1, max_sha1()}}
		]
	).

id_hex_int_conv_test() ->
	?assertEqual(max_sha1(), id_hex( id_int( max_sha1() ) )).
id_hex_int_noconv_hex_test() ->
	?assertEqual(max_sha1(), id_hex( max_sha1() ) ).

-endif.
