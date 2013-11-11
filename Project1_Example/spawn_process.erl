-module(spawn_process).

-export([start/1,main/2]).


-record(fragment,
	{
	f,
	i
	}).

start(Hf) ->
	io:format("Currently Spawned ~p ~n ",[Hf]),
	
	execute:initialize(Hf,[]). %custom spawn process function inside execute module
	
main([],P) ->
	io:format("Spawning Done..PID list: ~p ~n", [P]),
	g_main:getback(P);

main(X,Nodes) -> 
	
	PID = start(hd(X)), %take head of list and end to spawn
	
	main(X--[hd(X)],[PID|Nodes]).	 %recursive calls till the list is empty=[]



