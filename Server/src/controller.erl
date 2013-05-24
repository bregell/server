%% @author Johan
%% @doc This module will send out control signals to the different units,
%% Will need a way to acess the sockets from the listener to 
%% then send the data to the correct unit. 

-module(controller).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,input/0,ack_list/0,send/2,send/3]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the controller and registers the process that handles the list of sockets.
%% @end
start() ->
	case whereis(input) of 
		undefined ->
			register(input, spawn(?MODULE, input ,[]))
	end,
	monitor(process, input),
	case whereis(ack_list) of 
		undefined ->
			register(ack_list, spawn(?MODULE, ack_list ,[]))	
	end,
	monitor(process, ack_list),
	mailbox().
	

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

ack_list() ->
	ack_list([]).
	
ack_list(Data) ->
	receive
		{ack_request, {PowerStrip_SerialId, ReqestPid}} ->
			ack_list(lists:append(Data, [{PowerStrip_SerialId, ReqestPid}]));
		{find_ack, PowerStrip_SerialId} ->
			case lists:keyfind(PowerStrip_SerialId, 1, Data) of
				{PowerStrip_SerialId, ReqestPid} ->
					ReqestPid ! ok,
					ack_list(lists:delete({PowerStrip_SerialId,ReqestPid}, Data));
				false ->
					ack_list(Data)
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
		{ack, PowerStrip_SerialId} ->
			ack_list ! {find_ack, PowerStrip_SerialId};
		{'DOWN',_,process,{input,_},_} ->
			register(input, spawn(?MODULE, input ,[])),
			monitor(process, input);
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
			case gen_tcp:send(Socket, PowerStrip_SerialId++":"++Status++"\n") of
				ok ->
					PowerStrip_Id = integer_to_list(sql_builder:get_powerStripId(PowerStrip_SerialId)),
					SocketId = [integer_to_list(N) || {N} <- sql_builder:get_socketId(PowerStrip_Id)],
					Sql = sql_builder:new_status(SocketId, string:tokens(Status, ";")),
					odbc_unit ! {insert, Sql};
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
			case gen_tcp:send(Socket, PowerStrip_SerialId++":"++Status++"\n") of
				ok ->
					PowerStrip_Id = integer_to_list(sql_builder:get_powerStripId(PowerStrip_SerialId)),
					SocketId = [integer_to_list(N) || {N} <- sql_builder:get_socketId(PowerStrip_Id)],
					Sql = sql_builder:new_status(SocketId, string:tokens(Status, ";")),
					odbc_unit ! {insert, Sql},
					ack_list ! {ack_request, {PowerStrip_SerialId, self()}},
					io:fwrite("Waiting for ack\n"),
					%%receive 
					%%	ok ->
					%%		ack_sucess(RequestSocket)
					%%after 
					%%	3000 ->
					%%		ack_failed(RequestSocket)
					%%end;
					ack_sucess(RequestSocket);
				{error, _} ->
					io:fwrite("Could not send to: "++PowerStrip_SerialId++"\n"),
					ack_failed(RequestSocket)
			end;
		{not_found} ->  
			io:fwrite("Socket not found \n"),
			ack_failed(RequestSocket)
	end.
	
ack_sucess(RequestSocket) ->
	case gen_tcp:send(RequestSocket, "switchRequestTrue\n") of
		ok ->
			io:fwrite("Switch Request ok ack sent\n");
		{error, _} ->
			io:fwrite("Switch Request ok ack not sent\n")
	end.
	
ack_failed(RequestSocket) ->
	case gen_tcp:send(RequestSocket, "switchRequestFailed\n") of
		ok ->
			io:fwrite("Switch Request fail ack sent\n");
		{error, _} ->
			io:fwrite("Switch Request fail ack not sent\n")
	end.
