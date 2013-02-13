%% @author Johan
%% @doc Will look in the schedule every 5 minutes to start or
%% stop the different Units, sends the data to the command builder.


-module(schedule_worker).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	self() ! start,
	loop().
	
worker() ->
	receive
		start ->
			io:fwrite("TODO")
	end.

loop() ->
	Time = timer:minutes(5),
	timer:send_after(Time, start).


