%% @author Johan 
%% @doc The main module for the project, it will 
%% start and manange the listener and scheduler processes.

-module(server). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Port) ->
	odbc:start(),
	spawn_link(listener, start, [Port]),
	spawn_link(schedule_worker, start, []),	
	loop().

loop() ->
	receive
		end_server ->
			erlang:exit("Reason")
	end,
	loop().
