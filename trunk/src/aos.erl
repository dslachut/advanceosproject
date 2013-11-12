-module(aos).

-compile(export_all).

%% splitPattern() is a private function to return a list of non-word characters
splitPattern() ->
    [<<" ">>, <<"\t">>, <<"\n">>, <<".">>, <<",">>, <<"\"">>, <<"<">>, <<">">>,
    <<"?">>, <<":">>, <<";">>, <<"[">>, <<"]">>, <<"{">>, <<"}">>, <<"=">>,
    <<"+">>, <<"_">>, <<")">>, <<"(">>, <<"*">>, <<"&">>, <<"^">>, <<"%">>,
    <<"$">>, <<"#">>, <<"@">>, <<"!">>, <<"`">>, <<"~">>, <<"1">>, <<"2">>,
    <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"0">>].

%% toLower(S) converts ascii chars in a bitstring to lowercase
toLower(S) when is_bitstring(S) ->
    binary:list_to_bin(string:to_lower(binary:bin_to_list(S))).
    
%% longestWordLength(S) returns the bit-length of the longest word in S if S is 
%% a bitstring
longestWordLength(S) when is_bitstring(S) ->
    lists:max([erlang:bit_size(X) || X <- wordsIn(S)]).

%% longestWord(S) returns the longest word in S if S is a bitstring
longestWord(S) when is_bitstring(S) ->
    hd([X || X <- wordsIn(S), erlang:bit_size(X) == longestWordLength(S)]).

%% wordInList(W,L) returns true if a bitstring word exists in a list of 
%% bitstring words. This is a helper function for wordInString(S)
wordInList(_,[]) ->
    false;
wordInList(W,[H|T]) when is_bitstring(W), is_bitstring(H) ->
    case toLower(W) == toLower(H) of
        true -> true;
        false -> wordInList(W,T)
    end.
    
%% wordInString(W,S) returns true if word W is in bitstring S
wordInString(W,S) when is_bitstring(W), is_bitstring(S) ->
    wordInList(W, [X || X <- wordsIn(S), erlang:bit_size(X) > 0]).

%% wordsIn(S) returns a list of the words in bitstring S with no duplicates
wordsIn(S) when is_bitstring(S) ->
    sets:to_list(sets:from_list(
        [toLower(X) || 
            X <- binary:split(S,splitPattern(),[global]),
            erlang:bit_size(X) > 0]
    )).

%% wordFrequency(W,S) returns the integer count of occurences of bitstring word
%% W in bitstring S
wordFrequency(W,S) when is_bitstring(W), is_bitstring(S) ->
    length([X || 
        X <- binary:split(S,splitPattern(),[global]), 
        erlang:bit_size(X) > 0, 
        toLower(W) == toLower(X)
    ]).

%% wordFrequencies(S) returns a list of doubles (two-tuples) in which the first
%% element of each double is a word from bitstring S and the second element of 
%% each double is the number of occurences of that word in S
wordFrequencies(S) when is_bitstring(S) ->
    [{W,wordFrequency(W,S)} || W <- wordsIn(S), erlang:bit_size(W) > 0].
