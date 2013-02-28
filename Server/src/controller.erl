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
		{new, {SID,Socket}} ->
			case lists:keymember(SID, 1, Data)of
				true ->
					New_Data = lists:keyreplace(SID, 1, Data, {SID, Socket});
				false ->
					New_Data = lists:append(Data, [{SID, Socket}])
			end;
		{get_socket, {SID, Pid}} ->
			New_Data = Data,
			case lists:keyfind(SID, 1, Data) of
				{SID,Socket} ->
					Pid ! {found, Socket};
				false ->
					Pid ! {not_found}
			end
	end,
	input(New_Data).		

%% @doc
%% Waits for messages and relays them to the right process.
%% @end
mailbox() ->
	receive
		{new, {SID, Socket}} ->
			input ! {new, {SID, Socket}};
		{send, {SID, Status}} ->
			spawn_link(?MODULE, send, [SID, Status]);
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
send(SID, Status) ->
	input ! {get_socket, {SID, self()}},
	receive 
		{found, Socket} ->
			Format = fun(A) ->
							 case A of
								 N when N < 100 -> 
									   "0"++integer_to_list(A);
								 D when D >= 100 ->
									   integer_to_list(A)
							   end
					 end,
			Data = [Format(N) || N <- SID++":"++Status ],
			io:fwrite("Found\n"),
			case gen_tcp:send(Socket, Data) of
				ok ->
					io:fwrite("Sent\n");
				{error, _} ->
					io:fwrite("Could not send to: "++SID++"\n")
			end;
		{not_found} ->  
			io:fwrite("Socket not found \n")
	end.
