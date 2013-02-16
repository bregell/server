%% @author Johan
%% @doc This module will wait for tcp packets on specified port
%% and then send the data to the correct module with the correct data syntax.

-module(listener). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, server/1, worker/2, loop/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Port)->
	spawn_link(?MODULE, server, [Port]).

server(Port) ->
	case gen_tcp:listen(Port, [list, {active, false}, {packet, 0}]) of
		{ok, Listen} ->
			spawn_link(?MODULE, worker, [self(), Listen]),
			loop(Listen);
		{error, Reason} ->
			io:fwrite("Error cannot listen on port:"++Port++" Msg:" ++ Reason++"\n")
	end.
 
worker(Msg, Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			Msg ! new_worker,
			case gen_tcp:recv(Socket, 0) of
				{ok, Package} ->
					%% Parse string into 
					Output = string:tokens(Package, ":"),
					case Output of
						[SID,Data,Status] ->
							sql_builder:input([SID,string:tokens(Data, ";"),string:tokens(Status, ";")]);
						[SID,Status] ->
							controller:input([SID,string:tokens(Status, ";")]);
						_ ->
							io:fwrite("Error \n")
					end;
				{error, Reason} ->
					io:fwrite("Could not recieve "),
					io:fwrite(Reason),
					io:fwrite("\n"),
					Msg ! new_worker
			end;
		{error, Reason} ->
			io:fwrite("Could not accept "),
			io:fwrite(Reason),
			io:fwrite("\n"),
			Msg ! new_worker
	end.

loop(Listen) ->
	receive
		new_worker ->
			spawn_link(?MODULE, worker, [self(), Listen])
	end,
	loop(Listen).
