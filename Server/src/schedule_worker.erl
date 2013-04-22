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
			io:fwrite("Started Scheduling worker\n"),
			Send = fun(PowerStrip_SerialId, Socket, Status) -> 
				case Socket of
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
			{ok, [{selected,_,Timer_Rows}]} = sql_builder:get_timers(),
			{ok, [{selected,_,Repeater_Rows}]} = sql_builder:get_repeaters(),
			Rows = lists:append(Timer_Rows, Repeater_Rows),
			[Send(PowerStrip_SerialId, Socket, integer_to_list(Status)) || {PowerStrip_SerialId, Socket, Status}  <- Rows];
		_Else ->
			io:fwrite("Bad message\n")
	end,
	io:fwrite("Schedule worker finished\n"),
	worker().

loop(Pid) ->
	{_,{_,Min,_}} = calendar:now_to_local_time(now()),
	case (Min rem 5) of
		0 ->
			Pid ! start,
			timer:sleep(timer:minutes(4)),
			loop(Pid);
		_Else ->
			timer:sleep(timer:seconds(1)),
			loop(Pid)
	end.
	