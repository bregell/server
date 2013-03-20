%% @author Johan 
%% @doc Takes input in different formats and reformats the data
%% into SQL formated strings.

-module(sql_builder).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,input/1,select/6,update/4,insert/4,new_status/2,new_data/2]).



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
		[SID, Data, Status] ->
			%% Get the real PowerStrip_Id from the database
			SQL_1 = ["SELECT id FROM \"powerStrip_powerstrip\" WHERE \"serialId\"='"++SID++"'"],
			{ok, Answer_1} = odbc_unit:input(SQL_1),
			[{selected,_,[{Result}]}] = Answer_1,
			PowerStrip_Id = integer_to_list(Result),
	
			%% Build SQL string
			SQL_2 = lists:append(catch(new_data(PowerStrip_Id, Data)),catch(new_status(PowerStrip_Id, Status))),
			%% Send SQL to ODBC
			try (odbc_unit:input(SQL_2)) of
				{ok, Answer} ->
					{ok, Answer}
			catch
				{error, Reason} ->
					throw({error, Reason})	
			end;

		{SID, Length} ->
			%%SELECT id, socket_id, "powerStrip_id", "activePower", "timeStamp" FROM "powerStrip_consumption";
			SQL = ["SELECT \"powerStrip_id\", socket_id, \"timeStamp\", \"activePower\" 
					FROM \"powerStrip_consumption\"
					WHERE \"powerStrip_id\"='"++SID++"' 
					ORDER BY \"timeStamp\" DESC 
					LIMIT "++integer_to_list(Length),
			try (odbc_unit:input(SQL)) of
				{ok, Answer} ->
					{ok, Answer}
			catch	
				{error, Reason} ->
					throw({error, Reason})
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
new_data(PowerStrip_Id, Data) ->
	%% Get Id tags for each socket
	SQL = ["SELECT id FROM \"powerStrip_socket\" WHERE \"powerStrip_id\"="++PowerStrip_Id],
	{ok, Answer} = odbc_unit:input(SQL),
	[{_,_,Socket_Id}] = Answer,
	Id = [integer_to_list(N) || {N} <- Socket_Id],
	%%Id = ["1", "2", "3", "4"],
	
	%% A = Data, D = ActivePower
	%% (SID, Data[n], Status[n], NOW())
	%% Makes the Value fields for the SQL string
	Combine = fun(A) -> lists:map(fun(D) -> "('"++PowerStrip_Id++"','"++D++"',NOW())" end, lists:zipwith(fun(X, Y) -> X++"','"++Y end, A, Id)) end,
	
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
new_status(PowerStrip_Id, Status) ->
	%% List of Id tags for each socket
	Id = ["1", "2", "3", "4"],
	
	%% Makes a list of UPDATE statements for the given SID
    %% UPDATE "powerStrip_socket" SET id=?, socket=?, "powerStrip_id"=?, status=?, name=? WHERE <condition>;
	Make = fun(B) -> lists:map(fun({C,D}) -> "UPDATE \"powerStrip_socket\" SET status="++D++" WHERE \"powerStrip_id\"='"++PowerStrip_Id++"' AND socket='"++C++"'" end, lists:zip(Id, B)) end,
	Make(Status).
