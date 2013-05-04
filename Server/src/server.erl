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
	register(controller, spawn(controller, start, [])),
	monitor(process, controller),
	register(analyzer, spawn(analyzer, start, [])),
	monitor(process, analyzer),
	register(listener, spawn(listener, start, [Port])),
	monitor(process, listener),
	register(schedule, spawn(schedule_worker, start, [])),
	monitor(process, schedule),	
	loop(Port).

%% @doc
%% Top instance of the server handles all messages sent to top level.
%% @end
loop(Port) ->
	receive
		{'DOWN',_,process,{listener,_},_} ->
			register(listener, spawn(listener, start, [Port])),
			monitor(process, listener);
		{'DOWN',_,process,{controller,_},_} ->
			register(controller, spawn(controller, start, [])),
			monitor(process, controller);
		{'DOWN',_,process,{analyzer,_},_} ->
			register(analyzer, spawn(analyzer, start, [])),
			monitor(process, analyzer);
		{'DOWN',_,process,{schedule,_},_} ->
			register(schedule, spawn(schedule_worker, start, [])),
			monitor(process, schedule);
		_ ->
			io:fwrite("Message received in server top level \n")
	end,
	loop(Port).
