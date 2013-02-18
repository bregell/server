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
%% Input = 
%% [SID, Data, Status] where Data = [A,B,C,D] and Status = [A,B,C,D]
%% or [SID, ID] where ID = [A, B, C, D] 
%% @end
input(Input) ->	
	case Input of
		[SID, Data, Status] ->
			odbc_unit:input(new_data(SID, Data)),
			odbc_unit:input(new_status(SID, Status));
		[SID, ID] ->
			SQL = "SELECT * FROM consumption WHERE serialID='"++SID++"' AND id='"++ID++"' ORDER BY timestamp DESC LIMIT 120",
			case odbc_unit:input(SQL) of
				{selected, ColNames, Rows} ->
					io:fwrite(ColNames);
				{error, Reason} ->
					io:fwrite(Reason)
			end
	end.	

%% @doc 
%% Input = [Unit, Data, Status] where Data = [A,B,C,D] and Status = [A,B,C,D] 
%% @end
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
	["INSERT INTO consumption (serialId, activepower, id, timestamp) VALUES "++Values].

%% @doc 
%% Input = [SID, ID] where ID = [A, B, C, D]  
%% @end
new_status(SID, Status) ->
	%% List of Id tags for each socket
	Id = ["1", "2", "3", "4"],
	
	%% Makes a list of UPDATE statements for the given SID
	Make = fun(B) -> lists:map(fun({C,D}) -> "UPDATE socket SET status="++D++" WHERE serialID='"++SID++"' AND id='"++C++"'" end, lists:zip(Id, B)) end,
	Make(Status).
