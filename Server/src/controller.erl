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

start() ->
	Pid = spawn_link(?MODULE, input ,[[]]),
	register(input, Pid),
	mailbox().
	
%% list of {SID,Socket} 
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

%% Waits for messages
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

%% Gets and sends to the right unit
send(SID, Status) ->
	input ! {get_socket, {SID, self()}},
	receive 
		{found, Socket} ->
			io:fwrite("Found\n"),
			case gen_tcp:send(Socket, SID++":"++Status) of
				ok ->
					io:fwrite("Sent\n");
				{error, Reason} ->
					throw({error, Reason})
			end;
		{not_found} ->
			io:fwrite("Socket not found \n")
	end.
