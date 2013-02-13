%% @author Johan 
%% @doc The main module for the project, it will 
%% start and manange the listener and scheduler processes.

-module(server). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	odbc:start(),
		spawn_link(listener, start, []),
	loop().

loop() ->
	receive
		end_server ->
			erlang:exit("Reason")
	end,
	loop().
