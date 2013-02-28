%% @author Johan 
%% @doc Takes input in different formats and reformats the data
%% into SQL formated strings.

-module(sql_builder).

%% ====================================================================
%% API functions
%% ====================================================================
-export([input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 
%% Takes a preformatted input in the form of a list and converts it to SQL Strings.
%% @end
%% @spec (Input) -> ({ok, [Answer]} | {error, Reason})
%% Input = [string()]
%% Answer = ({updated, NRows} | {selected, Cols, Rows})
input(Input) ->	
	case Input of
		[SID, Data, Status] ->
			SQL = lists:append(catch(new_data(SID, Data)),catch(new_status(SID, Status))),
			try (odbc_unit:input(SQL)) of
				{ok, Answer} ->
					{ok, Answer}
			catch
				{error, Reason} ->
					throw({error, Reason})	
			end;
		{SID, Length} ->
			SQL = ("SELECT (powerStrip_id, socket_id, timestamp, activePower) FROM powerstrip_consumption WHERE powerStrip_id='"++SID++"' ORDER BY timestamp DESC LIMIT 0,"++integer_to_list(Length)),
			try (odbc_unit:input([SQL])) of
				{ok, Answer} ->
					{ok, Answer}
			catch	
				{error, Reason} ->
					throw({error, Reason})
			end
	end.

%% @doc
%% This function is called when the input is new data to be put into the table.
%% @end
%% @spec (SID, Data) -> [string()]
%% SID = string()
%% Data = [string()]
new_data(SID, Data) ->
	%% List of Id tags for each socket
	Id = ["1", "2", "3", "4"],
	
	%% A = SID, B = Data, C = Ports
	%% (SID, Data[n], Status[n], NOW())
	%% Makes the Value fields for the SQL string
	Combine = fun(A) -> lists:map(fun(D) -> "('"++SID++"','"++D++"',NOW())" end, lists:zipwith(fun(X, Y) -> X++"','"++Y end, A, Id)) end,
	
	%% From List to String
	Values = string:join(Combine(Data), ","),
	
	%% INSERT INTO consumption (serialId, id, activepower, timestamp) VALUES [(SID, Data[n], Status[n], NOW())]
	%% Adds the SQL syntax
	["INSERT INTO powerstrip_consumption (powerStrip_id, activePower, socket_id, timestamp) VALUES "++Values].

%% @doc
%% This function is called when the input is new statuses for the unit. 
%% @end
%% @spec (SID, Status) -> [string()]
%% SID = string()
%% Status = [string()]
new_status(SID, Status) ->
	%% List of Id tags for each socket
	Id = ["1", "2", "3", "4"],
	
	%% Makes a list of UPDATE statements for the given SID
	Make = fun(B) -> lists:map(fun({C,D}) -> "UPDATE powerstrip_socket SET status="++D++" WHERE powerStrip_id='"++SID++"' AND socket='"++C++"'" end, lists:zip(Id, B)) end,
	Make(Status).
