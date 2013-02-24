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

%% @doc
%% Starts the processes that need to run through out the whole time the server runs.
%% Then goes in to a loop and waits for messages.
%% @end
%% @spec (Port) -> (ok() | pid())
%% Port = port()
start(Port) ->
	odbc:start(),
	Pid_cont = spawn_link(controller, start, []),
	register(controller, Pid_cont),
	spawn_link(listener, start, [Port]),
	spawn_link(schedule_worker, start, []),	
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
