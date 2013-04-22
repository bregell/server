
-module(android).

%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/2,query/1, login/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================
decode([Package], Socket) ->
	io:fwrite("Package data: "++Package),
	Strip = fun(Str) -> string:sub_string(Str, 2, string:len(Str)-2) end,
	Input = Strip(Package),
	io:fwrite("Input data: "++Input++"\n"),
	List = [{A,B} || [A,B] <- [string:tokens(A, ":") || A <- string:tokens(Input, ",")]],
	case List of
		[{"username",UserName},{"request",Request}|Data] ->
			case Request of 
				"login" ->
					case Data of 
						[{"password",Password}] ->
							login(UserName, Password, Socket);
						_Else ->
							io:fwrite("Bad data for login request\n")
					end;
				"powerstrips" ->
					case Data of 
						[{"apikey",ApiKey}] ->
							getPowerStrips(UserName, ApiKey, Socket);
						_Else ->
							io:fwrite("Bad data for getPowerStrips request\n")
					end
			end;	
		[{"powerstripid",PowerStripId},{"request",Request}|Data] ->
			case Request of
				"sockets" ->
					case Data of
						[{"apikey", ApiKey}] ->
							getSockets(PowerStripId, ApiKey, Socket);
						_Else -> 
							io:fwrite("Bad data for getSockets request\n")
					end;
				"consumption" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"startdate",StartDate},
							{"enddate",EndDate}
						] ->
							getConsumptionPowerStrip(PowerStripId, ApiKey, Socket, StartDate, EndDate);
						_Else -> 
							io:fwrite("Bad data for getConsumptionPowerStrip request\n")
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
							getConsumptionSocket(SocketId, ApiKey, Socket, StartDate, EndDate);
						_Else -> 
							io:fwrite("Bad data for getConsumptionSocket request\n")
					end;
				"setname" ->
					case Data of
						[
							{"apikey", ApiKey},
							{"newname", NewName}
						] ->
							setName(SocketId, ApiKey, Socket, NewName);
						_Else -> 
							io:fwrite("Bad data for setName request\n")
					end;
				"ournon" ->
					case Data of
						[
							{"apikey", ApiKey}
						] ->
							switch(SocketId, ApiKey, Socket, "1");
						_Else -> 
							io:fwrite("Bad data for turnOn request\n")
					end;
				"turnoff" ->
					case Data of
						[
							{"apikey", ApiKey}
						] ->
							switch(SocketId, ApiKey, Socket, "0");
						_Else -> 
							io:fwrite("Bad data for turnOff request\n")
					end
			end
	end.
	
login(UserName, Password, Socket) ->
	Sql = 
	"
		select row_to_json(t)
		from
		(
			select '"++UserName++"' as username, apikey, 'true' as login 
			from auth_user
			where username = '"++UserName++"'
			and password = '"++Password++"'
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
					select k.id, \"serialId\" as serialid
					from (
						SELECT id, \"serialId\", user_id FROM \"powerStrip_powerstrip\"
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
						select pss.id, name, user_id
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
				and \"activePower\" BETWEEN '"++StartDate++"' AND '"++EndDate++"'
				group by \"timeStamp\", \"user_id\"
			) as atu
			on atu.user_id = au.id
			where au.apikey = '"++ApiKey++"'
			order by timestamp desc
		) as t
	",
	Result = query(Sql),
	Data = [ A || {A} <- Result],
	Message = "{powerstripid:\""+PowerStripId+"\", data:"++Data++"}",
	send(Socket, Message).
		
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
				and \"activePower\" BETWEEN '"++StartDate++"' AND '"++EndDate++"'
			) as atu
			on atu.user_id = au.id
			where au.apikey = '"++ApiKey++"'
			order by timestamp desc
		) as t
	",
	Result = query(Sql),
	Data = [ A || {A} <- Result],
	Message = "{socketid:\""++SocketId++"\", data:"++Data++"}",
	send(Socket, Message).
	
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
	case query(Sql_getUser) of
		[{SocketId}] ->
			case query(Sql_setName) of
				1 ->
					send(Socket, "{\"socketid\":\""++SocketId++"\",\"result\":true}");
				_Else ->
					io:fwrite("Error when trying to rename Socket\n"),
					send(Socket, "{\"socketid\":\""++SocketId++"\",\"result\":false}")
			end;
		_Else ->
			io:fwrite("Error when trying to rename Socket\n"),
			send(Socket, "{\"socketid\":\""++SocketId++"\",\"result\":false}")
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
		[{SerialID, SocketId, SocketNumber}] ->
			send(Socket, "{\"socketid\":\""++SocketId++"\",\"reuslt\":true}"),
			controlSocket(SerialID, SocketNumber, Switch);
		_Else ->
			io:fwrite("Error when trying to find PowerStrip_serialID\n")
	end.
	
queryAndSend(Sql, Socket) ->
	[{Message}] = query(Sql),
	send(Socket, Message).
	
query(Sql) ->
	{ok, Conn} = odbc:connect("DSN=PostgreSQL30", []),
	case (odbc:sql_query(Conn, Sql)) of
		{updated, N} ->
			N;
		{selected,_,N} ->
			N;
		{error, _} ->
			io:fwrite("Error at sql query\n")
	end.
	
send(Socket, Message) ->
	case gen_tcp:send(Socket, "Android#"++Message) of
			ok ->
				io:fwrite("Sent: Android#"++Message++"\n");
			{error, _} ->
				io:fwrite("Could not send Android#"++Message++"\n")
	end.
	
controlSocket(SerialID, 1, Mode) ->
	controller ! {send, {SerialID, Mode++";D;D;D"}};
controlSocket(SerialID, 2, Mode) ->
	controller ! {send, {SerialID, "D;"++Mode++";D;D"}};
controlSocket(SerialID, 3, Mode) ->
	controller ! {send, {SerialID, "D;D;"++Mode++";D"}};
controlSocket(SerialID, 4, Mode) ->
	controller ! {send, {SerialID, "D;D;D;"++Mode}}.
	