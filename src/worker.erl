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
                    clock}).

%% Data structure for a neighboring node
-record(neighbor, {pid,age=100000}).

%% Collection of data exchanged between two nodes
-record(share, {sender,
                neighbors,
                clock,
                longestWord}).

-record(search, {word,
                 repeats}).

%% Constant length of the neighbor list
c() -> 5.

%% Constant number of times to forward a search
s() -> 2.

%% Start running the node: construct the node data and start listening
startNode(Frag, FragNum, NList, Time, Master, IsNode0) ->
    Data = #node_data{fragment=Frag,
                      fragmentNum=FragNum,
                      wordFreqs=aos:wordFrequencies(Frag),
                      countedFrags=[FragNum],
                      longestWord=aos:longestWord(Frag),
                      neighbors=NList,
                      clock=Time,
                      master=Master},
    %Master ! {wfReport, FragNum, Data#node_data.wordFreqs},
    case IsNode0 of
        false ->
            FullData = Data#node_data{node0=hd(NList)},
            reportWF(FullData#node_data.node0, FragNum, Data#node_data.wordFreqs);
            %Data#node_data.node0 ! {wfReport, FragNum, Data#node_data.wordFreqs};
        true -> 
            AllWords = dict:new(),
            FullData = Data#node_data{allWords=dict:store(FragNum, Data#node_data.wordFreqs, AllWords)}
    end,
    listen(FullData).

%% infinitely respond to requests and exchange data
listen(Data) ->
    receive
        hello -> 
            io:format("Hello!~n"),
            %broadcast(hello, Data#node_data.neighbors),
            listen(incrementClock(Data));
        longestWord ->
            %io:format(Data#node_data.longestWord),
            Data#node_data.master ! Data#node_data.longestWord,
            listen(incrementClock(Data));
        die   -> 
            io:format("Dying ~p~n",[self()]),
            %Data#node_data.master ! dying,
            exit(self());
        alldie -> 
            %[X#neighbor.pid ! alldie || X <- Data#node_data.neighbors],
            broadcast(alldie,Data#node_data.neighbors),
            io:format("Dying ~p~n",[self()]),
            %Data#node_data.master ! dying,
            exit(self());
        neighbors ->
            io:format("~p~n",[Data#node_data.neighbors]),
            listen(incrementClock(Data));
        {exchange, Share} -> 
            Share#share.sender ! {reply, makeShare(Data)},
            NewData = combine(Share,Data),
            listen(incrementClock(NewData));
        {wfReport, N, WordFreqs} ->
            NewAllWords = dict:store(N, WordFreqs, Data#node_data.allWords),
            NewData = Data#node_data{allWords=NewAllWords},
            listen(incrementClock(NewData));
        {search, Search} ->
            %spawn(worker,forwardSearch,[Search, Data]),
            forwardSearch(Search,Data),
            findWord(Search,Data),
            listen(incrementClock(Data))
    after 1000 -> 
        NewData = initExchange(Data),
        listen(incrementClock(NewData))
    end.

%% Send a word frequency report to Node 0. Node 0 will then use this data.
reportWF(Node0, FragNum, Freqs) ->
    Node0#neighbor.pid ! {wfReport, FragNum, Freqs}.

%% If a search has repeats left, flood it to neighbors
forwardSearch(Search,Data) ->
    case Search#search.repeats < s() of
        false -> done;
        true ->
            Reps = Search#search.repeats + 1,
            NewSearch = Search#search{repeats=Reps},
            broadcast({search, NewSearch}, Data#node_data.neighbors)
    end.

broadcast(Message,[N|NList]) ->
    N#neighbor.pid ! Message,
    broadcast(Message, NList);
broadcast(_,[]) ->
    done.

%% Check if word is in the fragment at current node
findWord(Search,Data) ->
    case aos:wordInString(Search#search.word, Data#node_data.fragment) of
        true -> 
            %io:format("Found "),
            %io:format(Search#search.word),
            %io:format(" at ~p~n", [self()]),
            Data#node_data.master ! self();
        false -> done
    end.

%% Return node's data combined with received gossip
combine(Share,Data) ->
    NData = ncombine(Data,Share#share.neighbors),
    CData = ccombine(NData,Share#share.clock),
    LongWordData = lwcombine(CData,Share#share.longestWord), 
    LongWordData.

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
           longestWord=Data#node_data.longestWord}.

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
