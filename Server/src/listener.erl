%% @author Johan
%% @doc This module will wait for tcp packets on specified port
%% and then send the data to the correct module with the correct data syntax.

-module(listener). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, server/1, listener/2, loop/1, receiver/1]).



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
	server(Port).

%% @doc
%% The actual server starts a listener and goes into the loop
%% @end
%% @spec (Port) -> (pid() | string()) 
%% Port = inet:portnumber()
server(Port) ->
	case gen_tcp:listen(Port, [list, {active, false}, {packet, line}]) of
		{ok, Listen} ->
			spawn(?MODULE, listener, [self(), Listen]),
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
			?MODULE:receiver(Socket),
			gen_tcp:close(Socket),
			io:fwrite("\n");
		{error, Reason} ->
			Msg ! new_listener,
			[String,_] = Reason,
			io:fwrite("Could not accept "++String++"\n")
	end.

%% @doc
%% Waits for a package and when one is received it inteprets the data and passes
%% it to the right module, then goes back and waits for new packages.
%% @end
%% @spec (Msg, Socket) -> string()
%% Msg = pid() 
%% Socket = socket()
receiver(Socket) ->
	%%io:fwrite("Waiting for package\n"),
	%% @todo Add some good timeout value maybe.
	case gen_tcp:recv(Socket, 0) of
		{ok, Package} ->
			%% Parse string into list
			case string:tokens(Package, "#") of
				["Android"|Data] ->
					spawn(android, decode, [Data, Socket]);
				["PowerStrip"|Data] ->
					io:frite(Data),
					spawn(powerstrip, decode, [Data, Socket]);
				_Else ->
					%%io:fwrite(Package),
					Output = string:tokens(string:sub_string(Package, 1, string:len(Package)-1), ":"),
					%%io:fwrite(Output++"\n"),
					case Output of
						[PowerStrip_SerialId,Power,Status,Date,Time] ->
							%%io:fwrite(Time ++"\n"),
							controller ! {new,{PowerStrip_SerialId,Socket}},
							Power_list = string:tokens(Power, ";"),
							Status_list = string:tokens(Status, ";"),
							Date_list = string:tokens(Date, ";"),
							Time_list = string:tokens(Time, ";"),
							spawn(sql_builder,insert_from_powerstrip,[PowerStrip_SerialId,Power_list,Status_list,Date_list,Time_list]);							
							%%analyzer ! {read, PowerStrip_SerialId};
						[PowerStrip_SerialId, [79,75|_]] ->
							%%io:fwrite(Output++"\n"),
							%%io:fwrite("Ack recieved from power strip\n"),
							controller ! {ack, PowerStrip_SerialId};
						[PowerStrip_SerialId, Status] ->
							%%io:fwrite(Status++"\n"),
							controller ! {send,{PowerStrip_SerialId, Status, Socket}};
						_Else ->
							io:fwrite("Error no matching case, tcp packet thrown away.\n"),
							io:fwrite(Package),
							io:fwrite("\n")
					end
			end,
			?MODULE:receiver(Socket);
		{error, closed} ->
			%%controller ! {remove,{Socket}},
			io:fwrite("Socket closed\n");
		{error, _} ->
			%%controller ! {remove,{Socket}},
			io:fwrite("Could not recieve!\n")
	end.
	
%% @doc
%% Waits for messages, primarily to spawn a new listener. 
%% @end
%% @spec (Listen) -> (pid() | string())
%% Listen = socket()
loop(Listen) ->
	receive
		new_listener ->
			spawn(?MODULE, listener, [self(), Listen]),
			loop(Listen);
		_Else ->
			io:fwrite("Bad Msg\n"),
			loop(Listen)
	end.
	
