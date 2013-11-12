-module(aos).

-export([longestWordLength/1,longestWord/1,wordInString/2,toLower/1]).

%% splitPattern() is a private function to return a list of non-word characters
splitPattern() ->
    [<<" ">>, <<"\t">>, <<"\n">>, <<".">>, <<",">>, <<"\"">>, <<"<">>, <<">">>,
    <<"?">>, <<":">>, <<";">>, <<"[">>, <<"]">>, <<"{">>, <<"}">>, <<"=">>,
    <<"+">>, <<"_">>, <<")">>, <<"(">>, <<"*">>, <<"&">>, <<"^">>, <<"%">>,
    <<"$">>, <<"#">>, <<"@">>, <<"!">>, <<"`">>, <<"~">>, <<"1">>, <<"2">>,
    <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"0">>].

%% toLower(S) converts ascii chars in a bitstring to lowercase
toLower(S) ->
    binary:list_to_bin(string:to_lower(binary:bin_to_list(S))).
    
%% longestWordLength(S) returns the bit-length of the longest word in S if S is 
%% a bitstring
longestWordLength(S) ->
    lists:max([erlang:bit_size(X) || X <- binary:split(S,splitPattern(),
    [global])]).

%% longestWord(S) returns the longest word in S if S is a bitstring
longestWord(S) ->
    hd([X || X <- binary:split(S,splitPattern(),[global]), 
    erlang:bit_size(X) == longestWordLength(S)]).

%% wordInList(W,L) returns true if a bitstring word exists in a list of 
%% bitstring words. This is a helper function for wordInString(S)
wordInList(_,[]) ->
    false;
wordInList(W,[H|T]) ->
    case toLower(W) == toLower(H) of
        true -> true;
        false -> wordInList(W,T)
    end.
    
%% wordInString(W,S) returns true if word W is in bitstring S
wordInString(W,S) ->
    wordInList(W, [X || X <- binary:split(S,splitPattern(),[global]), 
    erlang:bit_size(X) > 0]).
