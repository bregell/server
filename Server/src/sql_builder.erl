%% @author Johan 
%% @doc Takes input in different formats and reformats the data
%% into SQL formated strings.

-module(sql_builder).

%% ====================================================================
%% API functions
%% ====================================================================
-export([insert_from_powerstrip/5,new_status/2,get_history/2,get_powerStripId/1,get_socketId/1,get_status/1,get_timers/0,get_repeaters/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================
	
get_powerStripId(PowerStrip_SerialId) ->
	Sql = "SELECT id FROM \"powerStrip_powerstrip\" WHERE \"serialId\" = '"++PowerStrip_SerialId++"'",
	odbc_unit ! {select, {self(), Sql}},
	receive
		{result, {selected, ["id"], [{Result}]}} ->
			Result
	end.
	
get_socketId(PowerStrip_Id) ->
	Sql = "SELECT id FROM \"powerStrip_socket\" WHERE \"powerStrip_id\" = '"++PowerStrip_Id++"' ORDER BY socket ASC",
	odbc_unit ! {select, {self(), Sql}},
	receive
		{result, {selected, ["id"], Result}} ->
			Result
	end.
	
get_history(PowerStrip_SerialId, Length) ->
	Sql = "
		SELECT socket, \"activePower\" FROM (
					SELECT socket_id, \"timeStamp\", \"activePower\" 
					FROM \"powerStrip_consumption\", \"powerStrip_powerstrip\"
					WHERE \"powerStrip_consumption\".\"powerStrip_id\"=\"powerStrip_powerstrip\".\"id\"
					AND \"powerStrip_powerstrip\".\"serialId\"='"++PowerStrip_SerialId++"' 
					ORDER BY \"timeStamp\" DESC 
					LIMIT "++integer_to_list(Length)++"
		) as t
		INNER JOIN \"powerStrip_socket\" as psp
		ON psp.id = t.socket_id
		ORDER BY socket ASC",
	odbc_unit ! {select, {self(), Sql}},
	receive
		{result, {selected,_Cols, Result}} ->
			Result
	end.

insert_from_powerstrip(PowerStrip_SerialId, Power, Status, Date, Time) ->
	%% Get the id of the powerStrip
	PowerStrip_Id = integer_to_list(get_powerStripId(PowerStrip_SerialId)),
	SocketId = [integer_to_list(S) || {S} <- get_socketId(PowerStrip_Id)],
	Sql = lists:append([new_data(PowerStrip_Id, SocketId, Power, Date, Time)], new_status(SocketId, Status)),
	odbc_unit ! {insert, Sql}.
	
%% @doc
%% This function is called when the input is new data to be put into the table.
%% @end
%% @spec (SID, Data) -> string()
%% SID = string()
%% Data = [string()]
%% Timestamp = string()
new_data(PowerStrip_Id, SocketId, Power, Date, Time) ->
	Timestamp = string:join(Date, "-") ++ " " ++ string:join(Time, ":"),
	
	Value_List = 
	["(
		'"++PowerStrip_Id++"',
		'"++P++"',
		'"++S++"',
		'"++Timestamp++"'
	)" || {S,P} <- lists:zip(SocketId, Power)],
	
	%% From List to String
	Values = string:join(Value_List, ","),
	
	%% INSERT INTO consumption (serialId, id, activepower, timestamp) VALUES [(SID, Data[n], Status[n], NOW())]
	%% Adds the SQL syntax
	"INSERT INTO \"powerStrip_consumption\" (\"powerStrip_id\", \"activePower\", socket_id, \"timeStamp\") VALUES "++Values.
	
	

%% @doc
%% This function is called when the input is new statuses for the unit. 
%% @end
%% @spec (SID, Status) -> [string()]
%% SID = string()
%% Status = [string()]
new_status(SocketId, Status) ->
	%% Makes a list of UPDATE statements for the given SID
    %% UPDATE "powerStrip_socket" SET id=?, socket=?, "powerStrip_id"=?, status=?, name=? WHERE <condition>;
	["UPDATE \"powerStrip_socket\" SET status="++S++" WHERE id = '"++SL++"'" || {S,SL} <- lists:zip(Status, SocketId), S /= "D"].


%% @doc
%% This function is called when the status of a PowerStrip is needed. 
%% @end
get_status(PowerStrip_SerialId) ->
	Sql = ["SELECT status 
			FROM \"powerStrip_socket\", \"powerStrip_powerstrip\" 
			WHERE \"powerStrip_socket\".\"powerStrip_id\"=\"powerStrip_powerstrip\".\"id\"
			AND \"powerStrip_powerstrip\".\"serialId\"='"++PowerStrip_SerialId++"'
			ORDER BY \"powerStrip_socket\".socket ASC"],
	odbc_unit ! {select, {self(), Sql}},
	receive
		{result, {selected, ["status"], Result}} ->
			Result
	end.

get_timers() ->
	Sql = [
<<<<<<< HEAD
		"	SELECT powerstrip_id, socket, mode 
			FROM
			(
				SELECT powerstrip_id, mode 
				FROM 	\"powerStrip_schedule_timer\" as pst, 
					\"powerStrip_powerstrip_schedule_timer\" as ppst 
				WHERE ppst.schedule_timer_id = pst.id	
				AND time BETWEEN (NOW() - INTERVAL '5' MINUTE) AND NOW()
			) as ppp
			INNER JOIN \"powerStrip_socket\" as ps
			ON ps.\"powerStrip_id\" = ppp.powerstrip_id
			UNION ALL
			SELECT \"powerStrip_id\" as powerstrip_id, socket, mode 
			FROM
			(
=======
		"SELECT \"serialId\", socket, mode FROM(
			SELECT socket, socket_id, mode, \"powerStrip_id\", status FROM(
>>>>>>> d935f9a60bb84f8fd9b4b9ba84ba5b88d6c0abae
				SELECT socket_id, mode 
				FROM 	\"powerStrip_schedule_timer\" as pst, 
					\"powerStrip_socket_schedule_timer\" as psst
				WHERE psst.schedule_timer_id = pst.id	
				AND time BETWEEN (NOW() - INTERVAL '5' MINUTE) AND NOW()
			) AS pss
			INNER JOIN \"powerStrip_socket\" as ps
			ON pss.socket_id = ps.id
		"
	],
	odbc_unit:input(Sql).
	
get_repeaters() ->
	Sql = [
		"SELECT \"serialId\", socket, socket_id, mode FROM
		(
			SELECT socket, socket_id, mode, \"powerStrip_id\" FROM
			(
				SELECT * FROM
				(
					SELECT id as schedule_repeat_id, mode FROM 
					(
						SELECT sr.id, activefrom, activeto, day, active, action_one_time, action_one_mode as mode FROM 
						\"powerStrip_schedule_day\" as sd, \"powerStrip_schedule_repeat\" as sr, \"powerStrip_schedule_repeat_days\" as srhd
						WHERE
						sr.id = srhd.repeat_id
						AND
						srhd.day_id = sd.id
						UNION 
						SELECT sr.id, activefrom, activeto, NULL as day, active, action_one_time, action_one_mode as mode FROM 
						\"powerStrip_schedule_repeat\" as sr, \"powerStrip_schedule_repeat_days\" as srhd
						WHERE 
						sr.id <> srhd.repeat_id
					) as one
					WHERE 
					active = true
					AND
					action_one_time BETWEEN (localtime - INTERVAL '3' MINUTE) AND (localtime + INTERVAL '2' MINUTE)
					AND(
						day IS NULL
						OR
						day = CAST(to_char(NOW(), 'Day') as int)
					)
					AND
					activefrom <= NOW()
					AND(
						activeto >= NOW()
						OR
						activeto IS NULL
					)
					UNION
					SELECT id as socket_id, mode FROM 
					(
						SELECT sr.id, activefrom, activeto, day, active, action_two_time, action_two_mode as mode FROM 
						\"powerStrip_schedule_day\" as sd, \"powerStrip_schedule_repeat\" as sr, \"powerStrip_schedule_repeat_days\" as srhd
						WHERE
						sr.id = srhd.repeat_id
						AND
						srhd.day_id = sd.id
						UNION 
						SELECT sr.id, activefrom, activeto, NULL as day, active, action_two_time, action_two_mode as mode FROM 
						\"powerStrip_schedule_repeat\" as sr, \"powerStrip_schedule_repeat_days\" as srhd
						WHERE 
						sr.id <> srhd.repeat_id
					) as two
					WHERE 
					active = true
					AND
					action_two_time BETWEEN (localtime - INTERVAL '3' MINUTE) AND (localtime + INTERVAL '2' MINUTE)
					AND(
					day IS NULL
					OR
					day = CAST(to_char(NOW(), 'Day') as int)
					)
					AND
					activefrom <= NOW()
					AND(
					activeto >= NOW()
					OR
					activeto IS NULL
					)
				) as repeat
				NATURAL JOIN \"powerStrip_socket_schedule_repeat\"
			) AS foo
			INNER JOIN \"powerStrip_socket\"
			ON socket_id = id
			WHERE mode <> status
		) AS bar
		INNER JOIN \"powerStrip_powerstrip\"
		ON \"powerStrip_id\" = id"
	],
	odbc_unit:input(Sql).
	