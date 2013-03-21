%% @author Johan 
%% @doc Takes a list of preformatted SQL strings and sends
%% each one of them to the database trough the ODBC connector.

-module(odbc_unit). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([input/1,bulk_input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 
%% Takes a list of SQL Strings and sends them to the database one by one.
%% @end
%% @spec (Sql) -> ({ok,[tuple()]} | {error, Reason})
%% Sql = [string()]
input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=PostgreSQL30", []),
	Function = fun(A) -> (try (odbc:sql_query(Conn, A)) of
							  {updated, N} ->
								{updated, N};
							  {selected, C, R} ->
								{selected, C, R};
							  {error, Reason} ->
								{error, Reason}
						 catch 
							 {error, Reason} ->
								{error, Reason}
						 end) end,
	Return = [Function(N) || N <- Sql],
	odbc:disconnect(Conn),
	{ok, Return}.

%% @doc 
%% Takes a list of SQL Strings and sends them to the database as a bulk operation.
%% Might be harder to find errors, but faster.
%% @end
%% @spec (Sql) -> ({ok,{updated, NRows}} | {ok,{selected, ColNames, Rows}} | {error, Reason})
%% Sql = [string()]
bulk_input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=DB", []),
	Bulk = string:join(Sql, ";"),
	try (odbc:sql_query(Conn, Bulk)) of
		{updated, NRows} ->
			odbc:disconnect(Conn),
			{ok, {updated, NRows}};
		{selected, ColNames, Rows} ->
			odbc:disconnect(Conn),
			{ok, {selected, ColNames, Rows}}
	catch
		{error, Reason} ->
			odbc:disconnect(Conn),
			{error, Reason}
	end.
