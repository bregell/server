%% @author Johan
%% @doc This module will send out control signals to the different units,
%% Will need a way to acess the sockets from the listener to 
%% then send the data to the correct unit. 

-module(controller).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,input/1,send/2]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the controller and registers the process that handles the list of sockets.
%% @end
start() ->
	Pid = spawn_link(?MODULE, input ,[[]]),
	register(input, Pid),
	mailbox().

%% @doc
%% Handles a list of tuples and waits for messages.
%% @end
%% @spec (Data) -> [tuple()] | message()
%% Data = [tuple()] | [] 
%% @todo Implement this in mnesia or orddict
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
			spawn_link(?MODULE, send, [PowerStrip_SerialId, Status]);
		_ ->
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
					io:fwrite("Sent: "),
					io:fwrite(PowerStrip_SerialId++":"++Status++"\n");
				{error, _} ->
					io:fwrite("Could not send to: "++PowerStrip_SerialId++"\n")
			end;
		{not_found} ->  
			io:fwrite("Socket not found \n")
	end.
