
-module(masterNode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).

-record(neighbor, {pid,age=100000}). %%% Controller program needs to implement this record type

-record(floodsearch, {word, repeats}). %%% Controller program needs to implement this record type

-record(fragment,{f,i}).

recloop() -> %Search module for flood search
receive
Msg -> io:format("Found the word at ~p~n",[Msg]),
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
FragNode0 = hd(FinalReplFrag),
Frag = {FragNode0#fragment.f,FragNode0#fragment.i},
Node0 = spawn(worker,startNode,[Frag,[],0,self(),true]),
{ok,Value} = io:fread("Enter String to update->", "~s"),
UpdateValue = erlang:list_to_binary(Value),
spawnWorkers(FinalReplFrag--[hd(FinalReplFrag)],Node0,UpdateValue).

spawnWorkers([],Node0,UpdateValue) ->
io:format("~n**********Longest Word in F*******************~n"),
Node0 ! longestWord, %%%%% Example of getting the longest word in the document
receive
Msg -> io:format("Longest word in Node 0: ~p~n",[Msg])
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
Msg1 -> io:format("Longest after gossip: ~p~n",[Msg1])
after
5000 -> io:format("~p~n",["Fail Longword2"])
end,
io:format("~n**********Frequent Word in F*******************~n"),
Node0 ! mostFrequentWord, %%%%% Example of getting the most frequent word in the document
timer:sleep(10000),
receive
Msg1a -> io:format("Most Frequent Word: ~p~n",[Msg1a])
after
5000 -> io:format("~p~n",["Fail Most Frequent Word"])
end,
Node0 ! {update, 4, UpdateValue}, %%%%% Example of updating the contents of fragment number 5
io:format("~n**********Updating content entered in fragment*******************~n"),
timer:sleep(10000),
Node0 ! longestWord,
receive
Msg0 -> io:format("Longword after update: ~p~n",[Msg0])
after
5000 -> io:format("~p~n",["Fail Longword New"])
end,
Node0 ! mostFrequentWord,
timer:sleep(10000),
receive
Msg0a -> io:format("Most Frequent Word after update: ~p~n",[Msg0a])
after
5000 -> io:format("~p~n",["Fail New Most Frequent Word"])
end,
Node0 ! {search, <<"thirteen">>}, %%%%% Example of searching for the word thirteen
io:format("~n**************Searching word using Method 2**************~n"), 
timer:sleep(10000),
receive
Msg2 -> io:format("Found word at ~p~n",[Msg2])
after
5000 -> io:format("~p~n",["Fail Search 1"])
end,
timer:sleep(10000),
Node0 ! {floodsearch, #floodsearch{word = <<"to">>,repeats = 0}}, %%%%% Example of doing a floodsearch for the word "to"
io:format("~n**************Searching word using Flood Search**************~n"),
recloop(),
timer:sleep(10000),
Node0 ! neighbors,
Node0 ! alldie, %%%%% Example of how do stop all the nodes
io:format("~n***************Nodes died***********~n"),
done;
spawnWorkers(List,Node0,UpdateValue) -> %Spawns Process for each node
	FragNode = hd(List),
	Frag = {FragNode#fragment.f,FragNode#fragment.i},
	spawn(worker,startNode,[Frag,[#neighbor{pid=Node0,age=0}],0,self(),false]),
	spawnWorkers(List--[hd(List)],Node0,UpdateValue).
	