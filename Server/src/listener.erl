%% @author Johan
%% @doc This module will wait for tcp packets on specified port
%% and then send the data to the correct module with the correct data syntax.

-module(listener). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, server/1, listener/2, loop/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts a listener and waits for connections and relays the received data 
%% to the right module.
%% @end
%% @spec (Port) -> pid()
%% Port = inet:portnumber() 
start(Port)->
	spawn_link(?MODULE, server, [Port]).

%% @doc
%% The actual server starts a listener and goes into the loop
%% @end
%% @spec (Port) -> (pid() | string()) 
%% Port = inet:portnumber()
server(Port) ->
	case gen_tcp:listen(Port, [list, {active, false}, {packet, 0}]) of
		{ok, Listen} ->
			spawn_link(?MODULE, listener, [self(), Listen]),
			loop(Listen);
		{error, Reason} ->
			io:fwrite("Error cannot listen on that port \n"),  
			io:fwrite(Reason),
			io:fwrite("\n")
	end.

%% @doc
%% Waits for a connection and when one is received sends a message to loop
%% to start a new instace of itself.
%% @end
%% @spec (Msg, Listen) -> string()
%% Msg = pid() 
%% Listen = socket()
listener(Msg, Listen) ->
	io:fwrite("Waiting for connection\n"),
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			Msg ! new_listener,
			receiver(Socket),
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:fwrite("Could not accept "),
			io:fwrite(Reason),
			io:fwrite("\n"),
			Msg ! new_worker
	end.

%% @doc
%% Waits for a package and when one is received it inteprets the data and passes
%% it to the right module, then goes back and waits for new packages.
%% @end
%% @spec (Msg, Socket) -> string()
%% Msg = pid() 
%% Socket = socket()
receiver(Socket) ->
	io:fwrite("Waiting for package\n"),
	case gen_tcp:recv(Socket, 0) of
		{ok, Package} ->
			io:fwrite("Recieve OK\n"),
			%% Parse string into list 
			Output = string:tokens(Package, ":"),
			case Output of
				[SID,Data,Status] ->
					io:fwrite(Package),
					io:fwrite("\n"),
					controller ! {new,{SID,Socket}},
					try (sql_builder:input([SID,string:tokens(Data, ";"),string:tokens(Status, ";")])) of
						{ok, _} ->
							io:fwrite("Data sent without problems!\n")
					catch
						{error, Reason} ->
							io:fwrite("Error when sending data!\n"),
							io:fwrite(Reason),
							io:fwrite("\n");
						_ ->
							io:fwrite("Strange things is happening!\n")
					end,
					analyzer ! {read, SID};
				[SID,Status] ->
					controller ! {send,{SID, Status}};
				_ ->
					io:fwrite("Error no matching case, tcp packet thrown away.\n"),
					io:fwrite(Package),
					io:fwrite("\n")
			end,
			receiver(Socket);
		{error, Reason} ->
			io:fwrite("Could not recieve!\n"),
			io:fwrite(Reason),
			io:fwrite("\n")
	end.
	
%% @doc
%% Waits for messages, primarily to spawn a new listener. 
%% @end
%% @spec (Listen) -> (pid() | string())
%% Listen = socket()
loop(Listen) ->
	receive
		new_listener ->
			spawn_link(?MODULE, listener, [self(), Listen]);
		_ ->
			io:fwrite("Bad Msg \n")
	end,
	loop(Listen).
