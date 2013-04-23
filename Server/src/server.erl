%% @author Johan 
%% @doc The main module for the project, it will 
%% start and manange the listener and scheduler processes.

-module(server). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,start/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the processes that need to run through out the whole time the server runs.
%% Then goes in to a loop and waits for messages.
%% @end
%% @spec () -> (ok() | pid())
start() ->
	start(39500).

%% @doc
%% Start/0 with custom port.
%% @end
%% @spec (Port) -> (ok() | pid())
%% Port = port()
start(Port) ->
	odbc:start(),
	register(controller, spawn_link(controller, start, [])),
	%%register(analyzer, spawn_link(analyzer, start, [])),
	register(listener, spawn_link(listener, start, [Port])),
	%%register(schedule, spawn_link(schedule_worker, start, [])),	
	loop().

%% @doc
%% Top instance of the server handles all messages sent to top level.
%% @end
loop() ->
	receive
		_ ->
			io:fwrite("Message received in server top level \n")
	end,
	loop().
