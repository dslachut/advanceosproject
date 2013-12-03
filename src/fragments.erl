
-module(fragments).

%% ====================================================================
%% API functions
%% ====================================================================
-export([split/2,f1/1,my_append/3,main/1,recappend/4,replication/4,longestWord/0,freqWord/0]).
-record(fragment,
	{
	f,
	i
	}).

%% ====================================================================
%% Internal functions
%% ====================================================================

printItems(File,Temp2)->
{ok, S} = file:open(File, write),
lists:foreach( fun(X) -> io:format(S, "~p.~n",[X]) end, Temp2),
file:close(S).

split(File,NoOfNodes) -> 						%Splits the file into unit lists.
	List = readFromFile:readFile(File),			%Calls a method in ReadFile for tokenizing the strings.
	put(list,List),
	TList = f1(List), 							%The entire list is divided into unit number lists.
	%new_file:print(list_to_binary(TList)),
	Result = trunc(length(TList)/(NoOfNodes-5)),%To reduce the fragment size to a number less than the number of nodes. Since trun is used, it produces one less than the actual node size.
	my_append(TList,[],Result).		 			%To Append the unit words into fragments of same size.
	

longestWord() ->
	List =  get(list),
	Word = string_Manip:longestWordList(List).

freqWord() ->
	List =  get(list),
	Word = string_Manip:wordFrequencies(List),
	Word2=lists:reverse(lists:sort(Word)).
	%printItems("sample.txt",Word2).
	

f1(List)->
	L=[lists:sublist(List, X, 1) || X <- lists:seq(1,length(List),1)].	%The entire list is divided into unit word lists. [hello,world,number] to [[hello],[world],[number]]

my_append([],Lf,Size) -> 						%Base case to the recursive case.													
		Lfx=main(Lf); 							%Lfx will return a list with fragment numbers added to it. The data structure defined at the top will have a fragment and a number added to it. 		
												%[{fragment,[hello],1}],{fragment,[world],2},{fragment,number],3}
my_append(L,Lf,Size)->
	L1=lists:sublist(L,Size), 					% Create a sublist of uniform size.
	L2 = lists:append(Lf,[lists:append(L1)]), 	% Append it to an empty list at first and recursively call itself.
	my_append(L--L1,L2,Size). 					% recursively create lists with appended words removed. [[....],[......],...,[......]]


main(L) ->		
	B=recappend(L,[],#fragment{f=0,i=0},1). 	%B is returned to the my_append([],Lf,Size) which is inturn returned to the main_Master

recappend([],L2,Px,I) -> 						% Base case for recursive case	
	Bx=L2; 										% The final list with fragments are returned to the called method.

recappend(L,L2,P2,I) ->
	P = P2#fragment{f=hd(L),i=I}, 				% Updates the existing fragment values of 0,0 to the actual fragment and the actual fragment number.
	NewList = lists:append(L2,[P]),				% Appends the updated fragment to the existing list.	
	recappend(L--[hd(L)],NewList,P,I+1).		% Appends the remaining list by recursively calling this function with incremented fragment number for each fragment.


replication(Final_List,ReplicatedL,NoOfNodes,RepSize) when RepSize==0->	% base case for the recursive case, and returns the replicated list to the called method.
	Return=ReplicatedL; 
	
replication(Final_List,ReplicatedL,NoOfNodes,RepSize) when RepSize>0 -> 
	Random = random:uniform(length(Final_List)),						%Picks a random fragment to replicate.
	RepElement = lists:nth(Random, Final_List),							%Extracts that random element number from the list.
	NewList = lists:append(ReplicatedL,[RepElement]),					%Appends that element to the existing list.
	L2= Final_List--[RepElement],										%Removes that element from the original list so that it is not replicated again
	replication(L2,NewList,NoOfNodes,RepSize-1).						%recursively calls itself for RepSize times. If there are 23 nodes in total, the initial fragment size is 
																		%19 and 4 random fragments from the 19 are replicated, bringing a total of 23.

%print([])->
%	ok;
%print([Item|List]) ->
%	io:format("~p~n",[Item]),
%	print(List).