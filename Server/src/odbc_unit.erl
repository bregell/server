%% @author Johan 
%% @doc Takes a list of preformatted SQL strings and sends
%% each one of them to the database trough the ODBC connector.

-module(odbc_unit). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,worker/1,input/1,bulk_input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	odbc:start(),
	spawn(?MODULE, worker, [self()]),
	spawn(?MODULE, worker, [self()]),
	spawn(?MODULE, worker, [self()]),
	spawn(?MODULE, worker, [self()]),
	mailbox().

query(Sql, Conn) ->
	[H|_] = Sql,
	case is_list(H) of
		true ->
			bulk_input(Sql, Conn);
		false ->
			input(Sql, Conn)
	end.
	
%% @doc 
%% Takes a list of SQL Strings and sends them to the database one by one.
%% @end
%% @spec (Sql) -> ({ok,[tuple()]} | {error, Reason})
%% Sql = [string()]
input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=PostgreSQL30", []),
	input(Sql, Conn).
	
input(Sql, Conn) ->
	odbc:sql_query(Conn, Sql).
	
%% @doc 
%% Takes a list of SQL Strings and sends them to the database as a bulk operation.
%% @end
%% Might be harder to find errors, but faster.
%% @spec (Sql) -> ({ok,{updated, NRows}} | {ok,{selected, ColNames, Rows}} | {error, Reason})
%% Sql = [string()]
bulk_input(Sql) ->
	{ok, Conn} = odbc:connect("DSN=PostgreSQL30", []),
	input(Sql, Conn).
	
bulk_input(Sql, Conn) ->
	Bulk = string:join(Sql, ";"),
	odbc:sql_query(Conn, Bulk).

mailbox() ->
	receive
		{request, Worker_Pid} ->
			receive
				{insert, Sql} ->
					Worker_Pid ! {insert, Sql},
					mailbox();
				{select, {Request_Pid, Sql}} ->
					Worker_Pid ! {select, {Request_Pid, Sql}},
					mailbox()
			end;
		{exit} ->
			ok;
		{new_worker} ->
			spawn(?MODULE, worker, [self()]),
			mailbox()
	end.

worker(Pool_Pid) -> 
	{ok, Conn} = odbc:connect("DSN=PostgreSQL30", []),
	worker(Pool_Pid, Conn).
	
worker(Pool_Pid, Conn) ->
	Pool_Pid ! {request, self()},
	receive
		{insert, Sql} ->
			query(Sql, Conn),
			worker(Pool_Pid, Conn);
		{select, {Request_Pid, Sql}} ->
			Result = query(Sql, Conn),
			Request_Pid ! {result, Result},
			worker(Pool_Pid, Conn);
		{exit} ->
			odbc:disconnect(Conn),
			ok
	end.