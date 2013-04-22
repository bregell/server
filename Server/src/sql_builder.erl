%% @author Johan 
%% @doc Takes input in different formats and reformats the data
%% into SQL formated strings.

-module(sql_builder).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,input/1,select/6,update/4,insert/4,get_status/1,new_status/2,new_data/3,get_timers/0,get_repeaters/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start()->
	mailbox().

%% @doc 
%% Takes a preformatted input in the form of a list and converts it to SQL Strings.
%% @end
%% @spec (Input) -> ({ok, [Answer]} | {error, Reason})
%% Input = [string()]
%% Answer = ({updated, NRows} | {selected, Cols, Rows})
input(Input) ->	
	case Input of
		[PowerStrip_SerialId, Data, Status, Date, Time] ->
			%% Create timestamp
			Timestamp = string:join(Date, "-") ++ " " ++ string:join(Time, ":"),
			%% Build SQL string
			Sql = lists:append(catch(new_data(PowerStrip_SerialId, Data, Timestamp)),catch(new_status(PowerStrip_SerialId, Status))),
			%% Send SQL to ODBC
			try (odbc_unit:input(Sql)) of
				{ok, Answer} ->
					{ok, Answer}
			catch
				{error, Reason} ->
					{error, Reason}	
			end;

		{PowerStrip_SerialId, Length} ->
			%%SELECT id, socket_id, "powerStrip_id", "activePower", "timeStamp" FROM "powerStrip_consumption";
			Sql = ["SELECT \"powerStrip_id\", socket_id, \"timeStamp\", \"activePower\" 
					FROM \"powerStrip_consumption\", \"powerStrip_powerstrip\"
					WHERE \"powerStrip_consumption\".\"powerStrip_id\"=\"powerStrip_powerstrip\".\"id\"
					AND \"powerStrip_powerstrip\".\"serialId\"='"++PowerStrip_SerialId++"' 
					ORDER BY \"timeStamp\" DESC 
					LIMIT "++integer_to_list(Length)],
			try (odbc_unit:input(Sql)) of
				{ok, Answer} ->
					{ok, Answer}
			catch	
				{error, Reason} ->
					{error, Reason}
			end
	end.

insert(Pid, Table, Columns, Values)->
	SQL = "INSERT INTO "++Table++" ("++Columns++") VALUES "++Values,
	Pid ! {result, SQL}.

select(Pid, Columns, Tables, Wheres, Order, Limit)->
	SQL = ("SELECT ("++Columns++") FROM "++Tables++" WHERE "++Wheres++"' ORDER BY "++Order++" LIMIT "++Limit),
	Pid ! {result, SQL}.

update(Pid, Tables, Sets, Wheres)->
	SQL = ("UPDATE "++Tables++" SET "++Sets++" WHERE "++lists:flatten([N++" AND " || N <- Wheres, N /= lists:last(Wheres)])++lists:last(Wheres)),
	Pid ! {result, SQL}.

mailbox() ->
	receive
		{insert, Pid, {SID, Data, Status}} ->
			spawn_link(?MODULE, input, [Pid, SID, Data, Status]);
		{select, Pid, {Columns, Tables, Wheres, Order, Limit}} ->
			spawn_link(?MODULE, select, [Pid, Columns, Tables, Wheres, Order, Limit]);
		{update, Pid, {Tables, Sets, Wheres}} ->
			spawn_link(?MODULE, update, [Pid, Tables, Sets, Wheres])
	end,
	mailbox().
		

%% @doc
%% This function is called when the input is new data to be put into the table.
%% @end
%% @spec (SID, Data) -> [string()]
%% SID = string()
%% Data = [string()]
%% Timestamp = string()
new_data(PowerStrip_SerialId, Data, Timestamp) ->
	%% Get Id tags for each socket
	SQL = ["SELECT \"powerStrip_socket\".id, \"powerStrip_powerstrip\".id 
			FROM \"powerStrip_socket\", \"powerStrip_powerstrip\" 
			WHERE \"powerStrip_socket\".\"powerStrip_id\"=\"powerStrip_powerstrip\".id 
			AND \"powerStrip_powerstrip\".\"serialId\" = '"++PowerStrip_SerialId++"' 
			ORDER BY \"powerStrip_socket\".socket ASC"],
	{ok, Answer} = odbc_unit:input(SQL),
	[{_,_,Answer_List}] = Answer,
	Id = [integer_to_list(N) || {N,_} <- Answer_List],
	
	%% A = Data, D = ActivePower
	%% (SID, Data[n], Status[n], NOW())
	%% Makes the Value fields for the SQL string
	[{_,PowerStrip_Id}|_] = Answer_List,
	Combine = fun(A) -> lists:map(fun(D) -> "('"++integer_to_list(PowerStrip_Id)++"','"++D++"','"++Timestamp++"')" end, lists:zipwith(fun(X, Y) -> X++"','"++Y end, A, Id)) end,
	
	%% From List to String
	Values = string:join(Combine(Data), ","),
	
	%% INSERT INTO consumption (serialId, id, activepower, timestamp) VALUES [(SID, Data[n], Status[n], NOW())]
	%% Adds the SQL syntax
	["INSERT INTO \"powerStrip_consumption\" (\"powerStrip_id\", \"activePower\", socket_id, \"timeStamp\") VALUES "++Values].

%% @doc
%% This function is called when the input is new statuses for the unit. 
%% @end
%% @spec (SID, Status) -> [string()]
%% SID = string()
%% Status = [string()]
new_status(PowerStrip_SerialId, Status) ->
	%% List of Id tags for each socket
	Id = [integer_to_list(N) || N <- lists:seq(1, length(Status))],
	%% Makes a list of UPDATE statements for the given SID
    %% UPDATE "powerStrip_socket" SET id=?, socket=?, "powerStrip_id"=?, status=?, name=? WHERE <condition>;
	["UPDATE \"powerStrip_socket\" SET status="++N++" WHERE socket="++D++" AND \"powerStrip_id\" IN (SELECT id FROM \"powerStrip_powerstrip\" WHERE \"serialId\"='"++PowerStrip_SerialId++"')" || {N,D} <- lists:zip(Status,Id), N /= "D"].


%% @doc
%% This function is called when the status of a PowerStrip is needed. 
%% @end
get_status(PowerStrip_SerialId) ->
	Sql = ["SELECT status 
			FROM \"powerStrip_socket\", \"powerStrip_powerstrip\" 
			WHERE \"powerStrip_socket\".\"powerStrip_id\"=\"powerStrip_powerstrip\".\"id\"
			AND \"powerStrip_powerstrip\".\"serialId\"='"++PowerStrip_SerialId++"'
			ORDER BY \"powerStrip_socket\".socket ASC"],
	odbc_unit:input(Sql).

get_timers() ->
	Sql = [
		"SELECT \"serialId\", socket, socket_id, mode FROM(
			SELECT socket, socket_id, mode, \"powerStrip_id\", status FROM(
				SELECT socket_id, mode 
				FROM schedule_timer, schedule_socket_has_timer 
				WHERE timer_id = id 
				AND active = TRUE 
				AND time BETWEEN (NOW() - INTERVAL '5' MINUTE) AND NOW()
			) AS foo
			INNER JOIN \"powerStrip_socket\"
			ON socket_id = id
			WHERE mode <> status
		) AS bar
		INNER JOIN \"powerStrip_powerstrip\"
		ON \"powerStrip_id\" = id"
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
					SELECT id as repeat_id, mode FROM 
					(
						SELECT sr.id, activefrom, activeto, day, active, action_one_time, action_one_mode as mode FROM 
						schedule_day as sd, schedule_repeat as sr, schedule_repeat_has_day as srhd
						WHERE
						sr.id = srhd.repeat_id
						AND
						srhd.day_id = sd.id
						UNION 
						SELECT sr.id, activefrom, activeto, NULL as day, active, action_one_time, action_one_mode as mode FROM 
						schedule_repeat as sr, schedule_repeat_has_day as srhd
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
						day = CAST(to_char(NOW(), 'D') as int)
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
						schedule_day as sd, schedule_repeat as sr, schedule_repeat_has_day as srhd
						WHERE
						sr.id = srhd.repeat_id
						AND
						srhd.day_id = sd.id
						UNION 
						SELECT sr.id, activefrom, activeto, NULL as day, active, action_two_time, action_two_mode as mode FROM 
						schedule_repeat as sr, schedule_repeat_has_day as srhd
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
					day = CAST(to_char(NOW(), 'D') as int)
					)
					AND
					activefrom <= NOW()
					AND(
					activeto >= NOW()
					OR
					activeto IS NULL
					)
				) as repeat
				NATURAL JOIN schedule_socket_has_repeat
			) AS foo
			INNER JOIN \"powerStrip_socket\"
			ON socket_id = id
			WHERE mode <> status
		) AS bar
		INNER JOIN \"powerStrip_powerstrip\"
		ON \"powerStrip_id\" = id"
	],
	odbc_unit:input(Sql).
	