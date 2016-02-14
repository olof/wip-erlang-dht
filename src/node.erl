-module(node).
-export([id/0, distance/2]).

%% TODO: Generate node id (not this static fake)
id() -> "98d832967f9dacbf60381657a6cd5e077cb8d3cb".
%% TODO: OTP behavior
%% TODO: joining network

%% Calculate the Kamelia distance between two nodes: The XOR of
%% two node IDs. The IDs should be provided as a hexadecimal
%% representation.
%%
%% XXX: note that there currently is no validation that the IDs
%%      are the same length (and thus, potentially within the
%%      same namespace), that is up to the caller to verify.
distance(Node1, Node2) -> id_bin(Node1) bxor id_bin(Node2).

%% id_{hex,bin}, converts to/from binary/hexadecimal representation
%id_hex(BinStr) -> integer_to_list(BinStr, 16).
id_bin(HexStr) -> list_to_integer(HexStr, 16).

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
			{id_bin(max_sha1()), {min_sha1(), max_sha1()}}
		]
	).

-endif.
