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
		_ ->
			io:fwrite("Message received in server top level \n")
	end,
	loop().
