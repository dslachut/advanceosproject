-module(rfragment2).
-export([main/1,f1/1,my_append/2,replication/2]).




main(File) -> 
	List = readBigFile:read_file(File), %file to list
	TList = f1(List), %concludes a list of unit lists
	my_append(TList,[]). %call my attend
	

f1(List)->
	
	L=[lists:sublist(List, X, 1) || X <- lists:seq(1,length(List),1)].
	
	
my_append([],Lf) -> 
%when we are done with TList, Lf has a random list of sublists.		
		Lfx=recordlisttry:main(Lf), %Lfx is the list B with given numbers to each fragment.
		replication(Lfx,[]); % create replication

my_append(L,Lf)->

	L1=lists:sublist(L,random:uniform(length(L))), % split the list randomly as [[unit]..[unit]]
	L2 = lists:append(Lf,[lists:append(L1)]), % connect L1 as [[...]]
	my_append(L--L1,L2). % recursively create a list with sublists inside [[....],[......],...,[......]]
	
replication([],ReplicatedL)->
	Return=ReplicatedL; % return a list with groups of subfragments.

replication(Final_List,ReplicatedL) -> 

	L=[hd(Final_List)] ++ lists:sublist(Final_List,random:uniform(length(Final_List))), %take head and a random part
	L2= Final_List--[hd(Final_List)],
	Set=sets:from_list(L),
	Res=lists:append(ReplicatedL, [sets:to_list(Set)]), %create random fragments union for each of the fragments.
	replication(L2,Res). 
