-module(aostest).

-compile(export_all).

-record(neighbor, {pid,age=100000}).

-record(search, {word,
                 repeats}).


test() ->
    F1 = <<"To be or not to be,">>,
    F2 = <<"that is the question.">>,
    F3 = <<"Whether it is nobler">>,
    F4 = <<"in the mind to suffer">>,
    F5 = <<"slings and arrows of">>,
    F6 = <<"outrageous fortune,">>,
    Node0 = spawn(worker,startNode,[F1,1,[],0,self(),true]),
    spawn(worker,startNode,[F2,2,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F4,4,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F1,1,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F2,2,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F3,3,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F4,4,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F5,5,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    spawn(worker,startNode,[F6,6,[#neighbor{pid=Node0,age=0}],0,self(),false]),
    Node0 ! longestWord,
    receive 
        Msg -> io:format("Longword 1: ~p~n",[Msg])
    after 
        5000 -> io:format("~p~n",["Fail Longword"])
    end,
    timer:sleep(10000),
    Node0 ! hello,
    Node0 ! neighbors,
    timer:sleep(1000),
    Node0 ! longestWord,
    timer:sleep(1000),
    receive 
        Msg1 -> io:format("Longword 2: ~p~n",[Msg1])
    after 
        5000 -> io:format("~p~n",["Fail Longword"])
    end,
    Node0 ! {search, #search{word = <<"fortune">>, repeats=0}},
    timer:sleep(1000),
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
    "".
