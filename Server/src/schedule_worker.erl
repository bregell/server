%% @author Johan
%% @doc Will look in the schedule every 5 minutes to start or
%% stop the different Units, sends the data to the command builder.


-module(schedule_worker).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,loop/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	spawn_link(?MODULE, loop, [self()]),
	worker().

%% @todo Implement connection with database and command builder
worker() ->
	receive
		start ->
			%io:fwrite("Started Scheduling worker\n"),
			Send = fun(PowerStrip_SerialId, Socket, Status) -> 
				io:fwrite(Socket),
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
			case sql_builder:get_timers() of 
				{selected,_, Timer_Rows} ->
					Timer_Rows;
				{error,_}->
					Timer_Rows = []
			end,
			%%case sql_builder:get_repeaters() of			
			%%	{selected,_,Repeater_Rows} ->
			%%		ok;
			%%	{error,_}->
			%%		Repeater_Rows = []
			%%end,
			%% serialID;socket,socket_id,mode
			%% "SN-0000003";1;9;1
			Rows = lists:append(Timer_Rows, []),
			[Send(PowerStrip_SerialId, Socket, integer_to_list(Mode)) || {PowerStrip_SerialId, Socket, Mode}  <- Rows];
		_Else ->
			io:fwrite("Bad message\n")
	end,
	%io:fwrite("Schedule worker finished\n"),
	worker().

loop(Pid) ->
	{_,{_,Min,_}} = calendar:now_to_local_time(now()),
	case (Min rem 5) of
		0 ->
			Pid ! start,
			timer:sleep(timer:minutes(5)),
			loop(Pid);
		_Else ->
			timer:sleep(timer:seconds(1)),
			loop(Pid)
	end.
	