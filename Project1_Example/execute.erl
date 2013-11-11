-module(execute).
-export([start/3,initial_process/1,gossip/2,process_gossip_data/2,initialize/2,createsinglelist/3]).



-record(message, {from, %% PID of the sender
                  function, %% atom representing the function to send to 
                  data,
			data2}). %% the secret being passed

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

%spawn processes in two VMs
createsinglelist([],NewL,FragL) ->
	io:format("Appended List for gossip ******* = ~p ~n",[NewL]),
	Data=initial_process(NewL),

	Coin = random:uniform(10),
	if Coin rem 2 > 0 ->	
		spawn(execute,start,[Data,[],FragL]);
		 %spawning along with Fragment Record
	
	true -> 
		spawn('slave@192.168.0.102',execute,start,[Data,[],FragL])
		 %spawning along with Fragment Record
	end;
createsinglelist(Fraglist,NewL,FragL) ->
	Head = hd(Fraglist),
	NewLT = lists:append(NewL,Head#fragment.f),
	createsinglelist(Fraglist--[Head],NewLT,FragL).


initialize(FragmentRec,FV) -> 
	
	io:format("Head of FragmentRec= ~p ~n",[hd(FragmentRec)]),		
	createsinglelist(FragmentRec,[],FragmentRec).
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Change FV to PV
start(Data,FV,FragRec) ->
	
	if Data#data.flag>0 ->
		
		L2 = FV#fv.view -- [self()],
		
		RN = lists:sublist(L2,random:uniform(length(L2)),1),	
		
	
		hd(RN) ! #message{function=flagrequest,from=self()}; %ask for flag and decide whether to pull or push

	true ->
		io:format("Else~n")
	end,		
		%io:format("AFTERELSE~n"),
	
	receive
	
	#message{function=fullview,data=FVX} -> %io:format("Full View Received ~p",[FVX]),
		FV2 = #fv{view=FVX},
		start(Data,FV2,FragRec);

	#message{function=gossip_start} ->	%gossip start trigger
		%io:format("Gossipstarts from here ~n"),
		NewData = Data#data{flag=1},
		start(NewData,FV,FragRec);


	#message{function=reqresult} -> %receive flag request and send to requestor
		io:format("@@@@@@@@@@@@@@@@@@@@@@MAX result= ~p ~n",[Data#data.max]),
		start(Data,FV,FragRec);
	
		
	#message{function=flagrequest,from=From} -> %receive flag request and send to requestor
		From ! #message{function=flagreceive,from=self(),data=Data#data.flag},
		start(Data,FV,FragRec);

	#message{function=datarequest,from=From} -> %receive data request and send to requestor
		From ! #message{function=datareceive,data=Data},
		start(Data,FV,FragRec);

	#message{function=datareceive, data=NData } -> %receive pushed data and process it and generate new data state
		Updated_Flag = Data#data.flag+1, 

		New_Data=process_gossip_data(NData,Data#data{flag=Updated_Flag}),
		start(New_Data,FV,FragRec);

	#message{function=flagreceive,from=From,data=RcData} ->

		if RcData<Data#data.flag -> %push
			From ! #message{function=datareceive,data=Data},
			start(Data,FV,FragRec);
		true -> %else
			
			From ! #message{function=datarequest,from = self()}, %pull
			start(Data,FV,FragRec)
		end
		
		

		

	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	initial_process(Fragment) ->
		Max = lists:max(Fragment),
		
		DataInitial = #data{max=Max,flag=0}.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
		

	gossip(FVT,From) -> %%%%%%NOT REQUIRED NOW
				
		RN = lists:sublist(FVT,random:uniform(length(FVT)),1),	
		io:format("****INSIDE GOSSIP..RN = ~p",[RN]),	


	
		RN ! #message{function=flagrequest,from=From}. %ask for flag and decide whether to pull or push
				
		

	process_gossip_data(Received_Data,My_Data)-> %code for 2 nodes max
			Max = max(Received_Data#data.max,My_Data#data.max),
			New_My_data = My_Data#data{max=Max}.


