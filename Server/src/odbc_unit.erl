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

%% @doc Sql = [SQL].
input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=DB", []),
	lists:foreach(fun(A) -> odbc:sql_query(Conn, A) end, Sql),
	odbc:disconnect(Conn).
