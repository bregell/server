%% @author Johan 
%% @doc Takes input from the listener and asks the 
%% database for the history of that unit.


-module(analyzer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, worker/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
	mailbox().

mailbox() ->
	receive
		{read,SID} ->
			spawn_link(?MODULE, worker, [SID])
	end,
	mailbox().

worker(SID) ->
	io:fwrite(sql_builder:input([SID])).
