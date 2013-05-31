
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
							{"duration",Duration},
							{"amount",Amount}
						
						] ->
							getConsumptionUser(UserName, ApiKey, Socket, Duration, Amount);
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
							{"duration",Duration},
							{"amount",Amount}
						
						] ->
							getConsumptionPowerStrip(PowerStripId, ApiKey, Socket, Duration, Amount);
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
					end;
				"status" ->
					case Data of 
						[{"apikey",ApiKey}] ->
							getPowerStripStatus(PowerStripId, ApiKey, Socket);
						_Else ->
							io:fwrite("Bad data for getPowerStripStatus request\n"),
							send(Socket, "{\"username\":\""++PowerStripId++"\",\"result\":false}")
					end
			end;	
		[{"socketid",SocketId},{"request",Request}|Data] ->
			case Request of
				"consumption" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"duration",Duration},
							{"amount",Amount}
						
						] ->
							getConsumptionSocket(SocketId, ApiKey, Socket, Duration, Amount);						
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
					end;
				"status" ->
					case Data of
						[
							{"apikey", ApiKey}
						] ->
							getSocketStatus(SocketId, ApiKey, Socket);
						_Else -> 
							io:fwrite("Bad data for getSocketStatus request\n"),
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
			select "++PowerStripId++" as powerstripid,
			(
				select array_to_json(array_agg(row_to_json(a))) as sockets
				from
				(
					select socketid, name, status
					from 
					(
						select pss.id as socketid, pss.name, user_id, status
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
						select status 
						from(
							(
								select 1 as status
								from \"powerStrip_consumption\" as pc
								where \"timeStamp\" BETWEEN (NOW() - INTERVAL '30') AND NOW()
								and pc.\"powerStrip_id\" = k.id
								order by socket_id asc
							)	
							union 
							select 0 as status
						) as status
						order by status desc
						limit 1
					),
					(
						select array_to_json(array_agg(row_to_json(a))) as sockets
						from
						(
							SELECT id as socketid, name, status 
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

getConsumptionUser(UserName, ApiKey, Socket, Duration, Amount) ->
	{Interval, Divider} = duration(Duration, Amount),
	Sql =
	"
		select sum(activepower), timestamp
		from 
		(
			select avg(\"activePower\")*("++Divider++") as activepower, date_trunc('"++Duration++"', \"timeStamp\") as timestamp, socket_id
			from \"powerStrip_consumption\"
			where \"powerStrip_id\" IN
			(
				select id
				from \"powerStrip_powerstrip\"
				where user_id =
					(
						select id 
						from auth_user
						where username = '"++UserName++"'
						and apikey = '"++ApiKey++"'
					)
			)
			and \"timeStamp\" BETWEEN (CURRENT_TIMESTAMP - INTERVAL '"++Interval++"')  AND CURRENT_TIMESTAMP
			group by date_trunc('"++Duration++"', \"timeStamp\"), socket_id
			order by socket_id asc
		) as socket
		group by timestamp
		order by timestamp asc		
	",
	Result = query(Sql),
	case Result of
		[] ->
			send(Socket, "{\"username\":"++UserName++", \"result\":nodata}");
		_Else ->
			Data = [ A || {A} <- Result],
			Array = string:join(Data, ","),
			send(Socket, "{\"username\":"++UserName++", \"data\":["++Array++"]}")
	end.
	
getConsumptionPowerStrip(PowerStripId, ApiKey, Socket, Duration, Amount) ->
	{Interval, Divider} = duration(Duration, Amount),
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select sum(activepower) as activepower, timestamp 
			from auth_user as au
			inner join
			(
				select avg(\"activePower\")*("++Divider++") as activepower, date_trunc('"++Duration++"', \"timeStamp\") as timestamp, user_id, socket_id
				from \"powerStrip_consumption\" as psc
				inner join \"powerStrip_powerstrip\" as psp
				on psc.\"powerStrip_id\" = psp.id
				where psc.\"powerStrip_id\" = '"++PowerStripId++"'
				and \"timeStamp\" BETWEEN (CURRENT_TIMESTAMP - INTERVAL '"++Interval++"')  AND CURRENT_TIMESTAMP
				group by date_trunc('"++Duration++"', \"timeStamp\"), user_id, socket_id
			) as atu
			on atu.user_id = au.id
			where au.apikey = '"++ApiKey++"'
			group by timestamp
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
		
getConsumptionSocket(SocketId, ApiKey, Socket, Duration, Amount) ->
	{Interval, Divider} = duration(Duration, Amount),
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select activepower, timestamp
			from auth_user as au
			inner join
			(
				select avg(\"activePower\")*("++Divider++") as activepower, date_trunc('"++Duration++"', \"timeStamp\") as timestamp, user_id 
				from \"powerStrip_consumption\" as psc
				inner join \"powerStrip_powerstrip\" as psp
				on psc.\"powerStrip_id\" = psp.id
				where psc.socket_id = '"++SocketId++"'
				and \"timeStamp\" BETWEEN (CURRENT_TIMESTAMP - INTERVAL '"++Interval++"')  AND CURRENT_TIMESTAMP
				group by date_trunc('"++Duration++"', \"timeStamp\"), user_id
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
					send(Socket, "{\"socketid\":"++SocketId++",\"result\":true,\"newname\":\""++NewName++"\"}");
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
					send(Socket, "{\"powerstripid\":"++PowerStripId++",\"result\":true,\"newname\":\""++NewName++"\"}");
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
	
getSocketStatus(SocketId, ApiKey, Socket) ->
	Sql = 
	"
	select row_to_json(answer) 
	from(
		select "++SocketId++" as socketid, status
		from (
			select status, user_id
			from (
				select status, \"powerStrip_id\" as id
				from \"powerStrip_socket\" 
				where id = '"++SocketId++"'
			) as ps
			inner join \"powerStrip_powerstrip\" as pp
			on ps.id = pp.id
		) as status
		inner join auth_user as au
		on au.id = status.user_id
		where apikey = '"++ApiKey++"'
	) as answer
	",
	queryAndSend(Sql, Socket).
	
getPowerStripStatus(PowerStripId, ApiKey, Socket) ->
	Sql = 
	"
	select row_to_json(answer) 
	from(
		select distinct "++PowerStripId++" as powerstripid, 1 as status
		from (
			select distinct user_id as ui, pc.socket_id as sid, pc.\"powerStrip_id\" as pid
			from \"powerStrip_consumption\" as pc, \"powerStrip_powerstrip\" as pp
			where \"timeStamp\" BETWEEN (NOW() - INTERVAL '30') AND NOW()
			and pc.\"powerStrip_id\" = '"++PowerStripId++"'
			and pc.\"powerStrip_id\" = pp.id
			order by socket_id asc	
		) as status
		inner join auth_user as au
		on au.id = status.ui
		where apikey = '"++ApiKey++"'
		union 
		select distinct "++PowerStripId++" as powerstripid, 0 as status
		from auth_user
		where apikey = '"++ApiKey++"'
		order by status desc
	) as answer
	limit 1
	",
	queryAndSend(Sql, Socket).
	
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
				%%io:fwrite("Sent: Android#"++Message++"\n"),
				io:fwrite("Sent to Android OK\n");
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
	
duration(Duration, Amount)->
	case Duration of
		"year" ->
			Interval = Amount++"Y",
			Divider = "8760";
		"month" ->
			Interval = Amount++Duration,
			Divider = "720";
		"day" ->
			Interval = Amount++"D",
			Divider = "24";
		"hour" ->
			Interval = Amount++"H",
			Divider = "1";
		"minute" ->
			Interval = Amount++Duration,
			Divider = "0.0167"
	end,
	{Interval, Divider}.
	