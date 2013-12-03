
-module(masterNode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).
-record(word,{pID,commWord}).
-record(fragment,
	{
	f,
	i
	}).

%% ====================================================================
%% Internal functions
%% ====================================================================

%Splits the file into fragments and calls spawnproc for spawning each process.
start(File,Nodes)->
Temp=fragments:split(File,Nodes), 						%It passes the file to be splitted and the no of nodes to start with.
ResultS = Nodes - length(Temp), 						%The number of fragments created M is always less than the number of nodes so as to allow for replication. Hence computing the replication factor for the remaining nodes
Temp2=fragments:replication(Temp,Temp,Nodes,ResultS),	%Produces one fragment for each node along with replication.
%printlength(Temp2), 									%This should be equal to the number of nodes.
S=fragments:longestWord(),
io:format("~n~s is the longest word",[S]),
Max=fragments:freqWord(),
Word=erlang:hd(Max),
io:format("~n~p Highest frequency length",[element(1,Word)]),
io:format("~n~p Highest frequency word",[element(2,Word)]).
%spawnproc(Temp2,Nodes,1,[]).							%Fragments are ready, time to start spanning.


%spawnproc([],Size,Tot,PIDs) when Size==0-> 				% recursive call base case.
%	io:format("~p Nodes created with unique fragments").
%spawnproc(Fragments,Size,Tot,PIDs) when Size>0->		%Spawns one process for each node. Code to be written for each node by Shrikanth
%	PID = spawn(execute,start,hd[Fragments]),			%Collects PIDs of created processes.
%	NewPIDS = lists:append(PIDs,[PID]),					%Puts the PIDS in list.
%	NewFragment = Fragments--hd[Fragments],				%Removes the fragment from list 
%	spawnproc(NewFragments,Size-1,NEWPIDs).				%and proceeds with recursive call.


printlength(Temp2)->									% The function prints the length of a list.
io:format("~p.~n Length is ",[length(Temp2)]).
	
%print([])->
%	ok;
%print([Item|List]) ->
%	io:format("~p~n",[Item]),
%	print(List).
%printItems(File,Temp2)->
 %{ok, S} = file:open(File, write),
  %      lists:foreach( fun(X) -> io:format(S, "~p.~n",[X]) end, Temp2),
   %     file:close(S).
