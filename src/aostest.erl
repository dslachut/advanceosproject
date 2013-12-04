-module(aostest).

-compile(export_all).

-record(neighbor, {pid,age=100000}).

-record(search, {word,
                 repeats}).


test() ->
    F1 = {<<"To be or not to be,">>, 1},
    F2 = {<<"that is the question.">>, 2},
    F3 = {<<"Whether it is nobler">>, 3},
    F4 = {<<"in the mind to suffer">>, 4},
    F5 = {<<"slings and arrows of">>, 5},
    F6 = {<<"outrageous fortune,">>, 6},
    Node0 = spawn(worker,startNode,[F1,[],0,self(),true]),
    io:format("Node0 PID: ~p~n",[Node0]),
    spawn(worker,startNode,[F2,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F4,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F1,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F2,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F3,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F4,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F5,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F6,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    Node0 ! longestWord,
    receive 
        Msg -> io:format("Longword 1: ~p~n",[Msg])
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
        Msg1 -> io:format("Longword 2: ~p~n",[Msg1])
    after 
        5000 -> io:format("~p~n",["Fail Longword2"])
    end,
    Node0 ! mostFrequentWord,
    timer:sleep(10000),
    receive 
        Msg1a -> io:format("Most Frequent Word: ~p~n",[Msg1a])
    after 
        5000 -> io:format("~p~n",["Fail Most Frequent Word"])
    end,
    Node0 ! {search, #search{word = <<"fortune">>, repeats=0}},
    timer:sleep(5000),
    receive
        Msg2 -> io:format("Found 'fortune' at ~p~n",[Msg2])
    after
        5000 -> io:format("~p~n",["Fail Search 1"])
    end,
    Node0 ! {search, #search{word = <<"to">>, repeats=0}},
    receive
        Msg3 -> io:format("Found 'to' at ~p~n",[Msg3])
    after
        5000 -> io:format("Found 'to' at ~p~n",["Fail Search 2a"])
    end,
    receive
        Msg4 -> io:format("Found 'to' at ~p~n",[Msg4])
    after
        5000 -> io:format("~p~n",["Fail Search 2b"])
    end,
    receive
        Msg5 -> io:format("Found 'to' at ~p~n",[Msg5])
    after
        5000 -> io:format("~p~n",["Fail Search 2c"])
    end,
    timer:sleep(10000),
    Node0 ! neighbors,
    <<>>.
