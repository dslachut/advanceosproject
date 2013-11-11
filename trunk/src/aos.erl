-module(aos).

-export([longestWordLength/1,longestWord/1]).

splitPattern() ->
    [<<" ">>,<<"\t">>,<<"\n">>,<<".">>,<<",">>,<<"\"">>].

%% longestWordLength(S) returns the bit-length of the longest word in S if S is 
%% a bitstring
longestWordLength(S) ->
    lists:max([erlang:bit_size(X) || X <- binary:split(S,splitPattern(),[global])]).

%% longestWord returns the longest word in S if S is a bitstring
longestWord(S) ->
    hd([X || X <- binary:split(S,splitPattern(),[global]), erlang:bit_size(X) == longestWordLength(S)]).
