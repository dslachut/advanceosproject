-module(aos).

-export([ generateNodes/1, activateRandomNode/2, killAllNodes/1]).

-record(node, {
      neighbour = [],
	  age=0,
	  fragmentForNode = []

}).

-record(fragment,{f,i}).

activateRandomNode(NodeId, TotalNodes) ->

	Pid = whereis(list_to_atom("Node_" ++ integer_to_list(NodeId))),
	if
		Pid == undefined -> 
			activateRandomNode(random:uniform(TotalNodes), TotalNodes);
		Pid == self() ->  
			activateRandomNode(random:uniform(TotalNodes), TotalNodes);
		true ->
			Pid ! {activate}
	end.

%% this function will kill all the nodes first, and then calls the generateThreads(), which will generate threads
generateNodes(N) ->
	TotalNodes = N,
	
	killAllNodes(N*N),
	generateThreads(N-1, TotalNodes).


%% this function will assign neighbours to the current node, and will generate and register the threads

generateThreads(N, TotalNodes) ->
	if N == -1 -> 
		io:format("I am done with Spawning all process ~n ~n");
%%		Node1 = random:uniform(TotalNodes),
	%%	lists:map(fun(X) -> activateRandomNode(X, TotalNodes) end, [Node1]);

	true -> 	
		CurrentPID = self(),
		NodeNum = list_to_atom("Node_" ++ integer_to_list(N)),

		
UnSortedNeigh = [list_to_atom("Node_" ++ integer_to_list((N+1) rem TotalNodes)), list_to_atom("Node_" ++ integer_to_list((N+2) rem TotalNodes)),list_to_atom("Node_" ++ integer_to_list((N+3) rem TotalNodes)), list_to_atom("Node_" ++ integer_to_list((N+4) rem TotalNodes))],

		random:seed(erlang:now()),
		MyState = #node{
					
						neighbour = lists:usort(UnSortedNeigh)
						
					},

		io:format("State : ~w ~w ~n ", [NodeNum, MyState]),

		register(NodeNum, spawn(gossip_avg, gossip, [MyState, N, TotalNodes, CurrentPID])),
		generateThreads(N-1, TotalNodes)
	end.




killAllNodes(TotalNodes) ->
	ProcessList = lists:map(fun(X) -> (list_to_atom("Node_" ++ integer_to_list(X))) end, lists:seq(0, TotalNodes)),
	lists:foreach(fun(X) -> triggerNodeOperation(X, killnode) end , ProcessList).

triggerNodeOperation(NodeId, Operation) ->
	Pid = whereis(NodeId),
	if
		Pid == undefined -> 
			ok;
		true -> 
			Pid ! {Operation}
	end.

