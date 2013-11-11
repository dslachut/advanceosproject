-module(g_main).
-export([main/1,getback/1]).


-record(message, {from, 
                  function,  
                  data,
			data2}). 

-record (data, {
		max=0,
		flag=0
		}).



-record (fv,
	{
	view
	}).


-record(fragment,
	{
	f,
	i
	}).

	main(File) ->
	Temp = rfragment2:main(File), %create fragments with index numbers and arrange fragments in groups
		
	spawn_process:main(Temp,[]). %Create processes with the fragments using all the other modules!!!
	
	getback(PID)->	%Receive the PID list after all processes are created
		sendpid(PID,PID),
		timer:sleep(2000),
	
		hd(PID) ! #message{function=gossip_start}, %GOSSIP START in node 1
		io:format("The Process IDs are = ~p ~n",[PID]), %For further computations as required by user

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER DEFINED
 
		timer:sleep(10000),
		hd(PID) ! #message{function=reqresult}. %Maximum
			
%Send in all the processes the full view-You have to send partial view!!
	sendpid([],FullV) ->
		io:format("Sending Complete");
	

	sendpid(P_list,FullV) -> 
		
			
		hd(P_list) ! #message{function=fullview,data=FullV},
		sendpid(P_list--[hd(P_list)],FullV).
	


