-module(readBigFile).
-export([read_file/1]).

read_file(File)->
{ok, Binary} = file:read_file(File),
S = string:tokens(binary_to_list(Binary), ", \r\n\t"),
[list_to_float(X) || X <- S].

	
