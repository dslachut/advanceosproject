
-module(masterNode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).

-record(neighbor, {pid,age=100000}). %%% Controller program needs to implement this record type

-record(floodsearch, {word, repeats}). %%% Controller program needs to implement this record type

-record(fragment,{f,i}).

recloop() ->
receive
Msg -> io:format("Found 'to' at ~p~n",[Msg]),
recloop()
after
10000 -> ok
end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%Splits the file into fragments and calls spawnproc for spawning each process.
start(File,Nodes)->
Temp=fragments:split(File,Nodes), 								%It passes the file to be splitted and the no of nodes to start with.
ResultS = Nodes - length(Temp), 								%The number of fragments created M is always less than the number of nodes so as to allow for replication. Hence computing the replication factor for the remaining nodes
FinalReplFrag=fragments:replication(Temp,Temp,Nodes,ResultS),	%Produces one fragment for each node along with replication.
printItems("sample.txt",FinalReplFrag),
FragNode0 = hd(FinalReplFrag),
Frag = {FragNode0#fragment.f,FragNode0#fragment.i},
Node0 = spawn(worker,startNode,[Frag,[],0,self(),true]),
{ok,Value} = io:fread("Enter String to update->", "~s"),
UpdateValue = erlang:list_to_binary(Value),
spawnWorkers(FinalReplFrag--[hd(FinalReplFrag)],Node0,UpdateValue,length(Temp)).

printItems(File,Temp2)->
{ok, S} = file:open(File, write),
lists:foreach( fun(X) -> io:format(S, "~p.~n",[X]) end, Temp2),
file:close(S).

spawnWorkers([],Node0,UpdateValue,N) ->
	io:format("~n*****************Searching for a word**********************"),
	Node0 ! {search, <<"states">>}, %%%%% Example of searching for the word "Invented"
	timer:sleep(10000),
	receive
		Msg2 -> io:format("~nFound 'word' at ~p~n",[Msg2])
		after
		10000 -> io:format("~p~n",["Fail Search 1"])
	end,
	io:format("~n*****************Longest word in the fragment**********************"),
	Node0 ! longestWord, %%%%% Example of getting the longest word in the document
	receive
		Msg -> io:format("~nLongword at Node 0: ~p~n",[Msg])
	after
		5000 -> io:format("~p~n",["Fail Longword1"])
	end,
timer:sleep(10000),
Node0 ! hello,
Node0 ! neighbors,
timer:sleep(5000),
Node0 ! longestWord,
timer:sleep(5000),
receive
Msg1 -> io:format("~nLongword after gossip: ~p~n",[Msg1])
after
5000 -> io:format("~p~n",["Fail Longword2"])
end,
io:format("~n*****************Most frequent word in F**********************"),
Node0 ! mostFrequentWord, %%%%% Example of getting the most frequent word in the document
timer:sleep(10000),
receive
Msg1a -> io:format("~nMost Frequent Word: ~p~n",[Msg1a])
after
5000 -> io:format("~p~n",["Fail Most Frequent Word"])
end,
io:format("~n*****************Updating the said fragment with new value**********************"),
Node0 ! {update, random:uniform(N), UpdateValue}, %%%%% Example of updating the contents of fragment number 5
timer:sleep(100000),
io:format("~n*****************Longest word after update**********************"),
Node0 ! longestWord,
receive
Msg0 -> io:format("~nLongword New: ~p~n",[Msg0])
after
5000 -> io:format("~p~n",["Fail Longword New"])
end,
io:format("~n*****************Most frequent word after update**********************"),
Node0 ! mostFrequentWord,
timer:sleep(10000),
receive
Msg0a -> io:format("~nNew Most Frequent Word: ~p~n",[Msg0a])
after
5000 -> io:format("~p~n",["Fail New Most Frequent Word"])
end,
timer:sleep(10000),
Node0 ! {floodsearch, #floodsearch{word = <<"to">>,repeats = 0}}, %%%%% Example of doing a floodsearch for the word "to"
recloop(),
timer:sleep(10000),
Node0 ! neighbors,
io:format("~n*****************Sending messages for nodes to die**********************"),
Node0 ! alldie, %%%%% Example of how do stop all the nodes
done;
spawnWorkers(List,Node0,UpdateValue,FragNum) ->
	FragNode = hd(List),
	Frag = {FragNode#fragment.f,FragNode#fragment.i},
	spawn(worker,startNode,[Frag,[#neighbor{pid=Node0,age=0}],0,self(),false]),
	spawnWorkers(List--[hd(List)],Node0,UpdateValue,FragNum).
