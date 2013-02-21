%% @author Johan 
%% @doc Takes a list of preformatted SQL strings and sends
%% each one of them to the database trough the ODBC connector.

-module(odbc_unit). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 
%% Takes a list of SQL Strings and sends them to the database.
%% @end
%% @spec (Sql) -> (ok() | {error, Reason})
%% Sql = [string()]
%% Reason = string()
input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=DB", []),
	lists:foreach(fun(A) -> odbc:sql_query(Conn, A) end, Sql),
	odbc:disconnect(Conn).
