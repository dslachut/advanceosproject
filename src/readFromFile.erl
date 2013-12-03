
-module(readFromFile).

%% ====================================================================
%% API functions
%% ====================================================================
-export([readFile/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
readFile(File)->												
	{ok,Binary}=file:read_file(File),						%Read the file contents to a binary variable
	S = string:tokens(binary_to_list(Binary), ", \r\n\t."),	%Tokenizes the file contents based on new line, tab space and commas.
	[list_to_bitstring(X) || X <- S].						%Creates a list from tokens.

