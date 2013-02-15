%% @author Johan
%% @doc Will look in the schedule every 5 minutes to start or
%% stop the different Units, sends the data to the command builder.


-module(schedule_worker).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,worker/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	Pid = spawn_link(?MODULE, worker, []),
	Pid ! start,
	loop(Pid).

%% @todo Implement connection with database and command builder
worker() ->
	receive
		start ->
			io:fwrite("TODO\n")
	end,
	worker().

loop(Pid) ->
	Time = timer:minutes(5),
	timer:send_after(Time, Pid, start),
	loop(Pid).
