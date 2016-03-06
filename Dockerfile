FROM debian:jessie
RUN apt-get update && apt-get install --no-install-recommends -y \
	erlang-base \
	erlang-eunit \
	rebar
WORKDIR /usr/src/erlang-kademlia
CMD sh -c 'rebar eunit'
ADD . /usr/src/erlang-kademlia
