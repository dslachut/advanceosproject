-module(worker).

-compile(export_all).

%% Data living on the node
-record(node_data, {fragment,
fragmentNum=-1,
wordFreqs=[],
countedFrags=[],
longestWord,
neighbors=[],
master,
allWords,
node0,
lastUpdate=undefined,
lastSearch=undefined,
clock}).

%% Data structure for a neighboring node
-record(neighbor, {pid,age=100000}).

%% Collection of data exchanged between two nodes
-record(share, {sender,
neighbors,
clock,
longestWord,
search,
update}).

-record(floodsearch, {word, repeats}).
-record(search, {word, id}).
-record(update, {fragment, fragmentNum, id}).

%% Constant length of the neighbor list
c() -> 6.

%% Constant number of times to forward a search
s() -> 2.

%% Start running the node: construct the node data and start listening
startNode({Frag, FragNum}, NList, Time, Master, IsNode0) ->
	Data = #node_data{	fragment=Frag,
						fragmentNum=FragNum,
						wordFreqs=aos:wordFrequencies(Frag),
						countedFrags=[FragNum],
						longestWord=aos:longestWord(Frag),
						neighbors=NList,
						clock=Time,
						master=Master},
case IsNode0 of
	false ->
		FullData = Data#node_data{node0=hd(NList)},
		reportWF(FullData#node_data.node0, FragNum, Data#node_data.wordFreqs);
	true ->
		AllWords = dict:new(),
		FullData = Data#node_data{allWords=dict:store(FragNum, Data#node_data.wordFreqs, AllWords),node0=#neighbor{pid=self()}}
end,
listen(FullData).

%% infinitely respond to requests and exchange data
listen(Data) ->
		receive
		hello ->
			io:format("Hello!~n"),
			%broadcast(hello, Data#node_data.neighbors),
			listen(incrementClock(Data));
		longestWord -> % message for getting Longest Word
			%io:format(Data#node_data.longestWord),
			Data#node_data.master ! Data#node_data.longestWord,
			listen(incrementClock(Data));
		die -> % exits from the system
			io:format("Dying ~p~n",[self()]),
			%Data#node_data.master ! dying,
			exit(self());
		alldie -> % broadcasts to neighbours and exits self
			%[X#neighbor.pid ! alldie || X <- Data#node_data.neighbors],
			broadcast(alldie,Data#node_data.neighbors),
			io:format("Dying ~p~n",[self()]),
			%Data#node_data.master ! dying,
			exit(self());
		neighbors ->	% prints neighbours at any point in time
			io:format("~p~n",[Data#node_data.neighbors]),
			listen(incrementClock(Data));
		mostFrequentWord ->	% gets the most frequent word in fragment
			W = aos:mostFrequentWord(Data#node_data.allWords),
			%io:format("~p~n",[W]),
			Data#node_data.master ! W,
			listen(incrementClock(Data));
		{exchange, Share} -> %Exchanges data with co-workers
			Share#share.sender ! {reply, makeShare(Data)},
			NewData = combine(Share,Data),
			listen(incrementClock(NewData));
		{wfReport, N, WordFreqs} -> % word freqeuncy report that is shared between co workers.
			NewAllWords = dict:store(N, WordFreqs, Data#node_data.allWords),
			NewData = Data#node_data{allWords=NewAllWords},
			listen(incrementClock(NewData));
		{floodsearch, Search} -> %floods the search requests if there are any more searches left
		%spawn(worker,forwardSearch,[Search, Data]),
			forwardSearch(Search,Data),
			findWord(#search{word = Search#floodsearch.word},Data),
			listen(incrementClock(Data));
		{search, Word} -> % Search for a word using gossip
			if
				Data#node_data.lastSearch == undefined -> ID = 1;
				true -> ID = Data#node_data.lastSearch#search.id + 1
			end,
			Query = #search{word=Word, id=ID},
			NewData = scombine(Data,Query),
			listen(incrementClock(NewData));
		{update, FragNum, NewFrag} -> % Updates the data fragment with NewFrag
			if Data#node_data.lastUpdate == undefined -> ID = 1;
				true -> ID = Data#node_data.lastUpdate#update.id + 1
			end,
			Update = #update{fragment=NewFrag, fragmentNum=FragNum, id=ID},
			NewData = ucombine(Data,Update),
			listen(incrementClock(NewData))
		after 1000 -> % If no message received, exchange data
			NewData = initExchange(Data),
			listen(incrementClock(NewData))
end.

%% Send a word frequency report to Node 0. Node 0 will then use this data.
reportWF(Node0, FragNum, Freqs) ->
	Node0#neighbor.pid ! {wfReport, FragNum, Freqs}.

%% If a search has repeats left, flood it to neighbors
forwardSearch(Search,Data) ->
	case Search#floodsearch.repeats < s() of
	false -> done;
	true ->
		Reps = Search#floodsearch.repeats + 1,
		NewSearch = Search#floodsearch{repeats=Reps},
	broadcast({floodsearch, NewSearch}, Data#node_data.neighbors)
end.

broadcast(Message,[N|NList]) ->
	N#neighbor.pid ! Message,
	broadcast(Message, NList);
	broadcast(_,[]) ->
done.

%% Check if word is in the fragment at current node
findWord(Search,Data) ->
	case aos:wordInString(Search#search.word, Data#node_data.fragment) of
	true -> Data#node_data.master ! self();
	false -> done
	end.

%% Return node data with update replaced
processUpdate(Data, Update) ->
	if
		Data#node_data.fragmentNum == Update#update.fragmentNum ->
		NewData = Data#node_data{fragment=Update#update.fragment,
		wordFreqs=aos:wordFrequencies(Update#update.fragment),
		longestWord=aos:longestWord(Update#update.fragment),
		lastUpdate=Update};
	true -> NewData = Data#node_data{lastUpdate=Update}
	end,
	if
		NewData#node_data.node0#neighbor.pid == self() ->
		NewData#node_data{allWords=dict:store(NewData#node_data.fragmentNum, NewData#node_data.wordFreqs, NewData#node_data.allWords)};
	true ->
	reportWF(NewData#node_data.node0, NewData#node_data.fragmentNum, NewData#node_data.wordFreqs),
	NewData
end.

%% Return node's data combined with received gossip
combine(Share,Data) ->
	NData = ncombine(Data,Share#share.neighbors),
	CData = ccombine(NData,Share#share.clock),
	LWData = lwcombine(CData,Share#share.longestWord),
	SData = scombine(LWData,Share#share.search),
	ucombine(SData,Share#share.update).

%% Return a node's data, accounting for the new update
ucombine(Data,Update) ->
	if
		Update == undefined ->
		Data;
		Data#node_data.lastUpdate == undefined ->
		processUpdate(Data,Update);
		Data#node_data.lastUpdate#update.id >= Update#update.id ->
		Data;
	true ->
		processUpdate(Data,Update)
	end.

%% Return a node's data, accounting for the new search query
scombine(Data,Query) ->
	if
		Query == undefined ->
		Data;
		Data#node_data.lastSearch == undefined ->
		findWord(Query, Data),
		Data#node_data{lastSearch=Query};
		Data#node_data.lastSearch#search.id >= Query#search.id ->
		Data;
	true ->
		findWord(Query, Data),
		Data#node_data{lastSearch=Query}
	end.

%% return node's data with new longest word
lwcombine(Data,LongWord) ->
	case bit_size(LongWord) > bit_size(Data#node_data.longestWord) of
		true -> Data#node_data{longestWord=LongWord};
		false -> Data
	end.

%% return node's data with new neighbors list
ncombine(Data,NNList) ->
	case length(Data#node_data.neighbors) >= c() - length(NNList) of
		true -> ONList = lists:sublist(lists:sort(fun worker:neighborSort/2, Data#node_data.neighbors), c() - length(NNList));
		false -> ONList = Data#node_data.neighbors
	end,
	NewNList = [N || N <- lists:append(NNList,ONList), N#neighbor.pid =/= self()],
	%io:format("Neighbors~n~p~n~p~n~p~n",[ONList,NNList,NewNList]),
	Data#node_data{neighbors=NewNList}.

%% return node's data with synched logical clock
ccombine(Data, Clock) ->
	case Clock > Data#node_data.clock of
		true -> Data#node_data{clock=Clock};
		false -> Data
	end.

%% select random neighbor from list of all neighbors
selectRandomNeighbor(NList) ->
	case length(NList) of
		0 -> null;
		1 ->
	N = hd(NList),
	N#neighbor.pid;
	_Other ->
	N = hd([X || {_,X} <- lists:sort([{random:uniform(), N} || N <- NList])]),
	N#neighbor.pid
end.

%% Initiate information exchange, and return new verion of data
initExchange(Data) ->
	%io:format("InitiatingExchange~n"),
	Share = makeShare(Data),
	Target = selectRandomNeighbor(Data#node_data.neighbors),
	case Target == null of
		true -> Target = Target;
		false -> Target ! {exchange, Share}
	end,
	receive
		{reply, Reply} ->
			combine(Reply,Data)
	after 5000 ->
		Data
	end.

%% Create the set of data to share in a gossip
makeShare(Data) ->
	#share{sender=self(),
	neighbors=makeNeighborList(Data#node_data.neighbors),
	clock=Data#node_data.clock,
	longestWord=Data#node_data.longestWord,
	search=Data#node_data.lastSearch,
	update=Data#node_data.lastUpdate}.

%% Create a sublist of neighbors to share
makeNeighborList(NList) ->
	case length(NList) of
		0 -> lists:append([#neighbor{pid=self(),age=0}],NList);
		1 -> lists:append([#neighbor{pid=self(),age=0}],NList);
		_Other ->
			lists:append([#neighbor{pid=self(),age=0}],lists:sublist(lists:sort(fun worker:neighborSort/2, NList), c() div 2))
	end.

%% Returns the node's data with incremented time values
incrementClock(Data) ->
	Clock = Data#node_data.clock,
	NList = Data#node_data.neighbors,
	NewNList = [#neighbor{pid=N#neighbor.pid, age=N#neighbor.age + 1} || N <- NList],
	NewData = Data#node_data{clock=Clock+1, neighbors=NewNList},
	NewData.

%% Compare two neighbors for sorting
neighborSort(A,B) ->
	A#neighbor.age < B#neighbor.age.