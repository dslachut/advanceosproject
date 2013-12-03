-module(aos).

-export([ generateNodes/2, activateRandomNode/2, findPow/2, killAllNodes/1]).

-record(node, {
   	fingerTableSize,
    neighbour = [],
   	lastGossipNum,
	busy,
	fragmentEnabled,
	r
}).



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


generateNodes(N, FragmentEnabled) ->
	TotalNodes = N,
	FingerTableSize=findPow(TotalNodes-1,0),
	killAllNodes(N*N),
	generateThreads(N-1, TotalNodes, FingerTableSize, FragmentEnabled).


generateThreads(N, TotalNodes, FingerTableSize, FragmentEnabled) ->
	if N == -1 -> 
		io:format("I am done with Spawning all process ~n ~n"),
		Node1 = random:uniform(TotalNodes),
		lists:map(fun(X) -> activateRandomNode(X, TotalNodes) end, [Node1]);

	true -> 	
		CurrentPID = self(),
		NodeNum = list_to_atom("Node_" ++ integer_to_list(N)),

		

	
		UnSortedNeigh= lists:map(fun(X)->(trunc(trunc(N+math:pow(2,X)) rem (trunc(math:pow(2,FingerTableSize)))) rem TotalNodes) end, 
										lists:seq(0, FingerTableSize-1)),

		random:seed(erlang:now()),
		MyState = #node{
						fingerTableSize = FingerTableSize,	
						neighbour = lists:usort(UnSortedNeigh),
						
						r = -1,
						lastGossipNum = 0,
						busy = 0,
						fragmentEnabled = FragmentEnabled},

		io:format("State : ~w ~w ~n ", [NodeNum, MyState]),

		register(NodeNum, spawn(gossip_avg, gossip, [MyState, N, TotalNodes, CurrentPID])),
		generateThreads(N-1, TotalNodes, FingerTableSize, FragmentEnabled)
	end.

findPow(TotalNodes, Value) ->
	A = math:pow(2,Value),
	if	TotalNodes < A -> Value;
		true ->	0 + findPow(TotalNodes, Value+1)
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

