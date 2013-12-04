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
                    clock}).

%% Data structure for a neighboring node
-record(neighbor, {pid,age=100000}).

%% Collection of data exchanged between two nodes
-record(share, {sender,
                neighbors,
                clock,
                longestWord}).

%% Constant length of the neighbor list
c() -> 5.

%% Start running the node: construct the node data and start listening
startNode(Frag, FragNum, NList, Time, Master) ->
    Data = #node_data{fragment=Frag,
                      fragmentNum=FragNum,
                      wordFreqs=aos:wordFrequencies(Frag),
                      countedFrags=[FragNum],
                      longestWord=aos:longestWord(Frag),
                      neighbors=NList,
                      clock=Time,
                      master=Master},
    listen(Data).

%% infinitely respond to requests and exchange data
listen(Data) ->
    receive
        hello -> 
            io:format("Hello!~n"),
            listen(incrementClock(Data));
        longestWord ->
            io:format(Data#node_data.longestWord),
            listen(incrementClock(Data));
        die   -> 
            Data#node_data.master ! dying,
            exit(self());
        alldie -> 
            [X ! alldie || {X,_} <- Data#node_data.neighbors],
            Data#node_data.master ! dying,
            exit(self());
        {exchange, Share} -> 
            Share#share.sender ! {reply, makeShare(Data)},
            NewData = combine(Share,Data),
            listen(incrementClock(NewData))
    after 1000 -> 
        NewData = initExchange(Data),
        listen(incrementClock(NewData))
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
    case length(Data#node_data.neighbors) >= 2 of
        true -> ONList = lists:sublist(lists:sort(neighborSort, Data#node_data.neighbors), c() - length(NNList));
        false -> ONList = Data#node_data.neighbors
    end,
    Data#node_data{neighbors=lists:append(NNList,ONList)}.

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
    case length(NList) >= 2 of
        false -> NList;
        true -> lists:append([#neighbor{pid=self(),age=0}],lists:sublist(lists:sort(neighborSort, NList), c() div 2))
    end.

%% Returns the node's data with incremented time values
incrementClock(Data) ->
    Clock = Data#node_data.clock,
    NList = Data#node_data.neighbors,
    NewNList = [#neighbor{pid=N#neighbor.pid, age=N#neighbor.age + 1} || N <- NList],
    NewData = Data#node_data{clock=Clock+1, neighbors=NewNList},
    NewData.

%%
neighborSort(A,B) ->
    A#neighbor.age < B#neighbor.age.