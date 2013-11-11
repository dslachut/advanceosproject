-module(aos).

-export([longestWord/1]).

splitPattern() ->
    [<<" ">>,<<"\t">>,<<"\n">>,<<".">>,<<",">>,<<"\"">>].

%% longestWordLength(S) returns the bit-length of the longest word in S if S is 
%% a bitstring
longestWordLength(S) ->
    lists:max([erlang:bit_size(X) || X <- binary:split(S,splitPattern(),[global])]).
