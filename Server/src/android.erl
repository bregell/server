
-module(android).

%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================
decode([Package], Socket) ->
	io:fwrite("Package data: "++Package),
	Strip = fun(Str) -> string:sub_string(Str, 2, string:len(Str)-2) end,
	List = [{A,B} || [A,B] <- [string:tokens(A, ":") || A <- string:tokens(Strip(Package), ",")]],
	case List of
		[{"username",UserName},{"request",Request}|Data] ->
			case Request of 
				"login" ->
					case Data of 
						[{"password",Password}] ->
							login(UserName, Password, Socket);
						_Else ->
							io:fwrite("Bad data for login request\n"),
							send(Socket, "{\"username\":\""++UserName++"\",\"result\":false}")
					end;
				"powerstrips" ->
					case Data of 
						[{"apikey",ApiKey}] ->
							getPowerStrips(UserName, ApiKey, Socket);
						_Else ->
							io:fwrite("Bad data for getPowerStrips request\n"),
							send(Socket, "{\"username\":\""++UserName++"\",\"result\":false}")
					end;
				"powerstripsandsockets" ->
					case Data of 
						[{"apikey",ApiKey}] ->
							getPowerStripsAndSockets(UserName, ApiKey, Socket);
						_Else ->
							io:fwrite("Bad data for getPowerStripsAndSockets request\n"),
							send(Socket, "{\"username\":\""++UserName++"\",\"result\":false}")
					end;
				"consumption" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"startdate",StartDate},
							{"enddate",EndDate}
						] ->
							[SDate,STime] = string:tokens(StartDate, " "),
							StartTimestamp = SDate++" "++string:join(string:tokens(STime, "-"), ":"),
							[EDate,ETime] = string:tokens(EndDate, " "),
							EndTimestamp = EDate++" "++string:join(string:tokens(ETime, "-"), ":"),
							getConsumptionUser(UserName, ApiKey, Socket, StartTimestamp, EndTimestamp);
						_Else -> 
							io:fwrite("Bad data for getConsumptionSocket request\n"),
							send(Socket, "{\"socketid\":"++UserName++",\"result\":false}")
					end
			end;	
		[{"powerstripid",PowerStripId},{"request",Request}|Data] ->
			case Request of
				"sockets" ->
					case Data of
						[{"apikey", ApiKey}] ->
							getSockets(PowerStripId, ApiKey, Socket);
						_Else -> 
							io:fwrite("Bad data for getSockets request\n"),
							send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":false}")
					end;
				"consumption" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"startdate", StartDate},
							{"enddate", EndDate}
						] ->
							%%{powerstripid:5,request:consumption,apikey:apikey1011,startdate:3913-06-09 00-00-00.000+0100,enddate:3913-06-10 00-00-00.000+0100}
							%%powerstripid:12,request:consumption,apikey:1a2b3c4d5e6f7g,startdate:2010-01-01 00-00-00.000+0000,enddate:2014-01-01 00-00-00.000+0000
							[SDate,STime] = string:tokens(StartDate, " "),
							StartTimestamp = SDate++" "++string:join(string:tokens(STime, "-"), ":"),
							[EDate,ETime] = string:tokens(EndDate, " "),
							EndTimestamp = EDate++" "++string:join(string:tokens(ETime, "-"), ":"),
							getConsumptionPowerStrip(PowerStripId, ApiKey, Socket, StartTimestamp, EndTimestamp);
						_Else -> 
							io:fwrite("Bad data for getConsumptionPowerStrip request\n"),
							send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":false}")
					end;
				"setname" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"newname", NewName}
						] ->
							setPsName(PowerStripId, ApiKey, Socket, NewName);
						_Else -> 
							io:fwrite("Bad data for setName request\n"),
							send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":false}")
					end
			end;	
		[{"socketid",SocketId},{"request",Request}|Data] ->
			case Request of
				"consumption" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"startdate",StartDate},
							{"enddate",EndDate}
						] ->
							[SDate,STime] = string:tokens(StartDate, " "),
							StartTimestamp = SDate++" "++string:join(string:tokens(STime, "-"), ":"),
							[EDate,ETime] = string:tokens(EndDate, " "),
							EndTimestamp = EDate++" "++string:join(string:tokens(ETime, "-"), ":"),
							getConsumptionSocket(SocketId, ApiKey, Socket, StartTimestamp, EndTimestamp);
						_Else -> 
							io:fwrite("Bad data for getConsumptionSocket request\n"),
							send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}")
					end;
				"setname" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"newname", NewName}
						] ->
							setName(SocketId, ApiKey, Socket, NewName);
						_Else -> 
							io:fwrite("Bad data for setName request\n"),
							send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}")
					end;
				"turnon" ->
					case Data of
						[
							{"apikey", ApiKey}
						] ->
							switch(SocketId, ApiKey, Socket, "1");
						_Else -> 
							io:fwrite("Bad data for turnOn request\n"),
							send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}")
					end;
				"turnoff" ->
					case Data of
						[
							{"apikey", ApiKey}
						] ->
							switch(SocketId, ApiKey, Socket, "0");
						_Else -> 
							io:fwrite("Bad data for turnOff request\n"),
							send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}")
					end
			end;
		_Else ->
			send(Socket, "{\"result\":false}")
	end.
	
login(UserName, Password, Socket) ->
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select '"++UserName++"' as username, apikey, true as login 
			from auth_user
			where username = '"++UserName++"'
			and androidpassword = '"++Password++"'
		) as t
	",
	queryAndSend(Sql, Socket).
	
getPowerStrips(UserName, ApiKey, Socket) ->
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select '"++UserName++"' as username,
			(
				select array_to_json(array_agg(row_to_json(d))) as powerstrips
				from
				(
					select k.id, \"serialId\" as serialid, k.name 
					from (
						SELECT id, \"serialId\", user_id, name FROM \"powerStrip_powerstrip\"
					) as k
					inner join auth_user 
					on user_id = auth_user.id
					where username = '"++UserName++"'
					and apikey = '"++ApiKey++"'
				) as d
			)
		) as t
	",
	queryAndSend(Sql, Socket).
	
getSockets(PowerStripId, ApiKey, Socket) -> 
	Sql = 
	"
		select row_to_json(d)
		from
		(
			select '"++PowerStripId++"' as powerstripid,
			(
				select array_to_json(array_agg(row_to_json(a))) as sockets
				from
				(
					select socketid, name
					from 
					(
						select pss.id as socketid, pss.name, user_id
						from \"powerStrip_socket\" as pss
						inner join \"powerStrip_powerstrip\" as psp
						on \"powerStrip_id\" = psp.id
						where \"powerStrip_id\" = '"++PowerStripId++"'
					) as k
					inner join auth_user as au
					on user_id = au.id
					where apikey = '"++ApiKey++"'
				) as a
			) as sockets
		) as d
	",
	queryAndSend(Sql, Socket).

getPowerStripsAndSockets(UserName, ApiKey, Socket) ->
	Sql =
	"
	select row_to_json(t)
	from
	(
		select '"++UserName++"' as username,
		(
			select array_to_json(array_agg(row_to_json(b))) as powerstrips
			from
			(
				select k.id, \"serialId\" as serialid, k.name, 
					(
						select array_to_json(array_agg(row_to_json(a))) as sockets
						from
						(
							SELECT id as socketid, name 
							FROM \"powerStrip_socket\" 
							WHERE \"powerStrip_id\" = k.id
						) as a
					) 
				from (
					SELECT id, \"serialId\", user_id, name FROM \"powerStrip_powerstrip\"
				) as k
				inner join auth_user 
				on user_id = auth_user.id
				where username = '"++UserName++"'
				and apikey = '"++ApiKey++"'
			) as b
		)
	) as t
	",
	queryAndSend(Sql, Socket).

getConsumptionUser(UserName, ApiKey, Socket, StartDate, EndDate) ->
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select sum(\"activePower\") as activepower, \"timeStamp\" as timestamp
			from \"powerStrip_consumption\" as psc
			inner join \"powerStrip_powerstrip\" as psp
			on psc.\"powerStrip_id\" = psp.id
			where psc.\"powerStrip_id\" IN (
				select k.id
				from (
					SELECT id, \"serialId\", user_id, name FROM \"powerStrip_powerstrip\"
				) as k
				inner join auth_user 
				on user_id = auth_user.id
				where username = '"++UserName++"'
				and apikey = '"++ApiKey++"'
			)
			and \"timeStamp\" BETWEEN '"++StartDate++"' AND '"++EndDate++"'
			group by \"timeStamp\", \"user_id\"
		) as t	
	",
	Result = query(Sql),
	case Result of
		[] ->
			send(Socket, "{\"username\":"++UserName++", \"result\":false}");
		_Else ->
			Data = [ A || {A} <- Result],
			Array = string:join(Data, ","),
			send(Socket, "{\"username\":"++UserName++", \"data\":["++Array++"]}")
	end.
	
getConsumptionPowerStrip(PowerStripId, ApiKey, Socket, StartDate, EndDate) ->
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select activepower, timestamp 
			from auth_user as au
			inner join
			(
				select sum(\"activePower\") as activepower, \"timeStamp\" as timestamp, user_id
				from \"powerStrip_consumption\" as psc
				inner join \"powerStrip_powerstrip\" as psp
				on psc.\"powerStrip_id\" = psp.id
				where psc.\"powerStrip_id\" = '"++PowerStripId++"'
				and \"timeStamp\" BETWEEN '"++StartDate++"' AND '"++EndDate++"'
				group by \"timeStamp\", \"user_id\"
			) as atu
			on atu.user_id = au.id
			where au.apikey = '"++ApiKey++"'
			order by timestamp asc
		) as t
	",
	Result = query(Sql),
	case Result of
		[] ->
			send(Socket, "{\"powerstripid\":"++PowerStripId++", \"result\":nodata}");
		_Else ->
			Data = [ A || {A} <- Result],
			Array = string:join(Data, ","),
			send(Socket, "{\"powerstripid\":"++PowerStripId++", \"data\":["++Array++"]}")
	end.
		
getConsumptionSocket(SocketId, ApiKey, Socket, StartDate, EndDate) ->
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select activepower, timestamp
			from auth_user as au
			inner join
			(
				select \"activePower\" as activepower, \"timeStamp\" as timestamp, user_id 
				from \"powerStrip_consumption\" as psc
				inner join \"powerStrip_powerstrip\" as psp
				on psc.\"powerStrip_id\" = psp.id
				where psc.socket_id = '"++SocketId++"'
				and \"timeStamp\" BETWEEN '"++StartDate++"' AND '"++EndDate++"'
			) as atu
			on atu.user_id = au.id
			where au.apikey = '"++ApiKey++"'
			order by timestamp desc
		) as t
	",
	Result = query(Sql),
	case Result of
		[] ->
			send(Socket, "{\"socketid\":"++SocketId++", \"result\":nodata}");
		_Else ->
			Data = [ A || {A} <- Result],
			Array = string:join(Data, ","),
			send(Socket, "{\"socketid\":"++SocketId++", \"data\":["++Array++"]}")
	end.
	
setName(SocketId, ApiKey, Socket, NewName) ->
	Sql_getUser = 
	"
		select socketid
		from 
		(
			select user_id, pss.id as socketid
			from \"powerStrip_socket\" as pss
			inner join \"powerStrip_powerstrip\" as psp
			on pss.\"powerStrip_id\" = psp.id
			where pss.id = '"++SocketId++"'
		) as t
		inner join auth_user as au
		on au.id = user_id
		where apikey = '"++ApiKey++"'
	",
	Sql_setName =
	"
		UPDATE \"powerStrip_socket\"
		SET name='"++NewName++"'
		WHERE id='"++SocketId++"'
	",
	{SocketId_Integer,_} = string:to_integer(SocketId),
	case query(Sql_getUser) of
		[{SocketId_Integer}] ->
			case query(Sql_setName) of
				1 ->
					send(Socket, "{\"socketid\":"++SocketId++",\"result\":true,\"newname\":"++NewName++"}");
				_Else ->
					io:fwrite("Error when trying to rename Socket\n"),
					send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}")
			end;
		_Else ->
			io:fwrite("Error when trying to rename Socket\n"),
			send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}")
	end.
	
setPsName(PowerStripId, ApiKey, Socket, NewName) ->
	Sql_getUser = 
	"
		select t.id
		from 
		\"powerStrip_powerstrip\" as t
		inner join auth_user as au
		on au.id = t.user_id
		where 
		t.id = '"++PowerStripId++"'
		and
		apikey = '"++ApiKey++"'
	",
	Sql_setName =
	"
		UPDATE \"powerStrip_powerstrip\"
		SET name='"++NewName++"'
		WHERE id='"++PowerStripId++"'
	",
	{PowerStripId_Integer,_} = string:to_integer(PowerStripId),
	case query(Sql_getUser) of
		[{PowerStripId_Integer}] ->
			case query(Sql_setName) of
				1 ->
					send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":true,\"newname\":"++NewName++"}");
				_Else ->
					io:fwrite("Error when trying to rename PowerStrip\n"),
					send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":false}")
			end;
		_Else ->
			io:fwrite("Error when trying to rename PowerStrip\n"),
			send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":false}")
	end.

switch(SocketId, ApiKey, Socket, Switch) ->
	Sql_serialId = 
	"
		select serialid, socketid, socket
		from 
		(
			select user_id, \"serialId\" as serialid, pss.id as socketid, socket
			from \"powerStrip_socket\" as pss
			inner join \"powerStrip_powerstrip\" as psp
			on pss.\"powerStrip_id\" = psp.id
			where pss.id = '"++SocketId++"'
		) as t
		inner join auth_user
		on id = user_id
		where apikey = '"++ApiKey++"'
	",
	case query(Sql_serialId) of
		[{SerialId, _SocketId, SocketNumber}] ->
			controlSocket(SerialId, SocketNumber, Switch, Socket);
		_Else ->
			send(Socket, "{\"socketid\":"++SocketId++",\"result\":false}"),
			io:fwrite("Error when trying to find PowerStrip_serialId\n")
	end.
	
queryAndSend(Sql, Socket) ->
	Result = query(Sql),
	case Result of 
		[{Message}] ->
			send(Socket, Message);
		[] ->
			send(Socket, "{\"status\":\"failed\"}")
	end.
	
query(Sql) ->
	{ok, Conn} = odbc:connect("DSN=PostgreSQL30", []),
	case (odbc:sql_query(Conn, Sql)) of
		{updated, N} ->
			N;
		{selected,_,N} ->
			N;
		{error, _} ->
			io:fwrite("Error at sql query\n"),
			[]
	end.
	
send(Socket, Message) ->
	case gen_tcp:send(Socket, "Android#"++Message++"\n") of
			ok ->
				%%io:fwrite("Sent: Android#"++Message++"\n");
				io:fwrite("OK\n");
			{error, _} ->
				io:fwrite("Could not send Android#"++Message++"\n")
	end.
	
controlSocket(SerialID, 1, Mode, Socket) ->
	controller ! {send, {SerialID, Mode++";D;D;D", Socket}};
controlSocket(SerialID, 2, Mode, Socket) ->
	controller ! {send, {SerialID, "D;"++Mode++";D;D", Socket}};
controlSocket(SerialID, 3, Mode, Socket) ->
	controller ! {send, {SerialID, "D;D;"++Mode++";D", Socket}};
controlSocket(SerialID, 4, Mode, Socket) ->
	controller ! {send, {SerialID, "D;D;D;"++Mode, Socket}}.
	