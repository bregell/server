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
%% @spec (Sql) -> (ok() | {error, Reason})
%% Sql = [string()]
input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=DB", []),
	try (lists:foreach(fun(A) -> throw(odbc:sql_query(Conn, A)) end, Sql)) of
		{updated, NRows} ->
			{ok, {updated, NRows}};
		{selected, ColNames, Rows} ->
			{ok, {selected, ColNames, Rows}}
	catch
		{error, Reason} ->
			throw({error, Reason})
	end,
	odbc:disconnect(Conn).

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
			{ok, {updated, NRows}};
		{selected, ColNames, Rows} ->
			{ok, {selected, ColNames, Rows}}
	catch
		{error, Reason} ->
			throw({error, Reason})
	end,
	odbc:disconnect(Conn).
