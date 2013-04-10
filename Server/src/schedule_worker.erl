%% @author Johan
%% @doc Will look in the schedule every 5 minutes to start or
%% stop the different Units, sends the data to the command builder.


-module(schedule_worker).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,worker/0,timer/0,schedule/0]).



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
			spawn_link(?MODULE, timer, []),
			spawn_link(?MODULE, schedule, []);
		_ ->
			io:fwrite("Bad message\n")
	end,
	worker().

loop(Pid) ->
	timer:sleep(timer:minutes(5)),
	Pid ! start,
	loop(Pid).
	
timer() ->
Send = fun(PowerStrip_SerialId, Socket, Status) 
	-> case Socket of
		1 ->
			controller ! {send, {PowerStrip_SerialId, Status++";D;D;D"}};
		2 ->
			controller ! {send, {PowerStrip_SerialId, "D;"++Status++";D;D"}};
		3 ->
			controller ! {send, {PowerStrip_SerialId, "D;D;"++Status++";D"}};
		4 ->
			controller ! {send, {PowerStrip_SerialId, "D;D;D"++Status}}
	end
end,
{ok, [{selected,_,Rows}]} = sql_builder:get_timers(),
[Send(PowerStrip_SerialId, Socket, integer_to_list(Status)) || {PowerStrip_SerialId, Socket, Status}  <- Rows].

schedule() ->
	io:fwrite("To be inmplemented\n").