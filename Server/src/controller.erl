%% @author Johan
%% @doc This module will send out control signals to the different units,
%% Will need a way to acess the sockets from the listener to 
%% then send the data to the correct unit. 

-module(controller).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,input/0,send/2,send/3]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the controller and registers the process that handles the list of sockets.
%% @end
start() ->
	case whereis(input) of 
		undefined ->
			register(input, spawn(?MODULE, input ,[])),
			mailbox();
		_Else ->
			mailbox()
	end.
	

%% @doc
%% Handles a list of tuples and waits for messages.
%% @end
%% @spec (Data) -> [tuple()] | message()
%% Data = [tuple()] | [] 
%% @todo Implement this in mnesia or orddict
input() ->
	input([]).
	
input(Data) ->
	receive
		{new, {PowerStrip_SerialId,Socket}} ->
			case lists:keymember(PowerStrip_SerialId, 1, Data)of
				true ->
					input(lists:keyreplace(PowerStrip_SerialId, 1, Data, {PowerStrip_SerialId, Socket}));
				false ->
					input(lists:append(Data, [{PowerStrip_SerialId, Socket}]))
			end;
		{get_socket, {PowerStrip_SerialId, Pid}} ->
			case lists:keyfind(PowerStrip_SerialId, 1, Data) of
				{PowerStrip_SerialId, Socket} ->
					Pid ! {found, Socket},
					input(Data);
				false ->
					Pid ! {not_found},
					input(Data)
			end
	end.		

%% @doc
%% Waits for messages and relays them to the right process.
%% @end
mailbox() ->
	receive
		{new, {PowerStrip_SerialId, Socket}} ->
			input ! {new, {PowerStrip_SerialId, Socket}};
		{send, {PowerStrip_SerialId, Status}} ->
			spawn(?MODULE, send, [PowerStrip_SerialId, Status]);
		{send, {PowerStrip_SerialId, Status, RequestSocket}} ->
			spawn(?MODULE, send, [PowerStrip_SerialId, Status, RequestSocket]);
		_->
			io:fwrite("Bad Data\n")
	end,
	mailbox().

%% @doc
%% Sends a message to the input process with the SID and then waits for the answer.
%% @end
%% @spec (SID, Status) -> (string() | {error, Reason})
%% SID = string()
%% Status = string()
send(PowerStrip_SerialId, Status) ->
	input ! {get_socket, {PowerStrip_SerialId, self()}},
	receive 
		{found, Socket} ->
			io:fwrite("Found\n"),
			case gen_tcp:send(Socket, PowerStrip_SerialId++":"++Status) of
				ok ->
					spawn(odbc_unit, input, [sql_builder:new_status(PowerStrip_SerialId, string:tokens(Status, ";"))]),
					io:fwrite("Sent: "),
					io:fwrite(PowerStrip_SerialId++":"++Status);
				{error, _} ->
					io:fwrite("Could not send to: "++PowerStrip_SerialId++"\n")
			end;
		{not_found} ->  
			io:fwrite("Socket not found \n")
	end.
	
send(PowerStrip_SerialId, Status, RequestSocket) ->
	input ! {get_socket, {PowerStrip_SerialId, self()}},
	receive 
		{found, Socket} ->
			io:fwrite("Found\n"),
			case gen_tcp:send(Socket, PowerStrip_SerialId++":"++Status) of
				ok ->
					spawn(odbc_unit, input, [sql_builder:new_status(PowerStrip_SerialId, string:tokens(Status, ";"))]),
					io:fwrite("Sent: "),
					io:fwrite(PowerStrip_SerialId++":"++Status),
					case gen_tcp:send(RequestSocket, "switchRequestTrue\n") of
						ok ->
							io:fwrite("Switch Request ok ack sent\n");
						_Else ->
							io:fwrite("Switch Request ok ack not sent\n")
					end;
				{error, _} ->
					io:fwrite("Could not send to: "++PowerStrip_SerialId++"\n"),
					case gen_tcp:send(RequestSocket, "switchRequestFailed\n") of
							ok ->
							io:fwrite("Switch Request fail ack sent\n");
						_Else ->
							io:fwrite("Switch Request fail ack not sent\n")
					end
			end;
		{not_found} ->  
			io:fwrite("Socket not found \n"),
			case gen_tcp:send(RequestSocket, "switchRequestFailed\n") of
				ok ->
					io:fwrite("Switch Request fail ack sent\n");
				_Else ->
					io:fwrite("Switch Request fail ack not sent\n")
			end
	end.
