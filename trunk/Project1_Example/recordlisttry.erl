-module(recordlisttry).
-export([main/1,recappend/4]).
-record(fragment,
	{
	f,
	i
	}).


main(L) ->
	
	
	B=recappend(L,[],#fragment{f=0,i=0},1). %B = Bx and return to rfragment2.

recappend([],L2,Px,I) ->

	Bx=L2; %Bx contains all the subfragments of the list with a given number in each of them.

recappend(L,L2,P2,I) ->
	P = P2#fragment{f=hd(L),i=I}, %create fragment record
	NewList = lists:append(L2,[P]),
	
	recappend(L--[hd(L)],NewList,P,I+1). %increase I number
