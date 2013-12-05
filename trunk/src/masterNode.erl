
-module(masterNode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).
%-record(word,{pID,commWord}).
%-record(fragment,{f,i}).
%-record(message,{from,fn,neigh}).

%% ====================================================================
%% Internal functions
%% ====================================================================

%Splits the file into fragments and calls spawnproc for spawning each process.
start(File,Nodes)->
Temp=fragments:split(File,Nodes), 								%It passes the file to be splitted and the no of nodes to start with.
ResultS = Nodes - length(Temp), 								%The number of fragments created M is always less than the number of nodes so as to allow for replication. Hence computing the replication factor for the remaining nodes
FinalReplFrag=fragments:replication(Temp,Temp,Nodes,ResultS),	%Produces one fragment for each node along with replication.
io:format("~p~n",[FinalReplFrag]).

%%spawnpro(Frag,S)-> 				% recursive call base case.
%%PID = goss:sp(hd(Frag)),
%%PID2 = goss:sp(hd(Frag)),
%%PID!#message{from=PID,fn=getNeigh,neigh=PID2},
%%PID2!#message{from=PID2,fn=getNeigh,neigh=PID},
%%timer:sleep(8000),
%%PID!#message{from=PID,fn=getWord,neigh=[S]}.
%goss:dispVal().

