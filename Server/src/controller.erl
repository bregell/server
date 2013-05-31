%% @author Johan
%% @doc This module will send out control signals to the different units,
%% Will need a way to acess the sockets from the listener to 
%% then send the data to the correct unit. 

-module(controller).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,input/0,ack_list/0,send/5]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the controller and registers the process that handles the list of sockets.
%% @end
start() ->
	Input = spawn_link(?MODULE, input ,[]),
	erlang:monitor(process, Input),
	Ack_List = spawn_link(?MODULE, ack_list ,[]),
	erlang:monitor(process, Ack_List),
	mailbox(Input, Ack_List).
	

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
		{remove, {Socket}} ->
			input(lists:keydelete(Socket, 2, Data));
		{get_socket, {PowerStrip_SerialId, ReqestPid}} ->
			case lists:keyfind(PowerStrip_SerialId, 1, Data) of
				{PowerStrip_SerialId, Socket} ->
					ReqestPid ! {found, Socket},
					input(Data);
				false ->
					ReqestPid ! {not_found},
					input(Data)
			end
	end.

ack_list() ->
	ack_list([]).
	
ack_list(Data) ->
	receive
		{ack_request, {PowerStrip_SerialId, ReqestPid}} ->
			%%io:fwrite("Ack Request:"++PowerStrip_SerialId++"\n"),
			ack_list(lists:append(Data, [{PowerStrip_SerialId, ReqestPid}]));
		{ack_checkout, PowerStrip_SerialId} ->
			case lists:keyfind(PowerStrip_SerialId, 1, Data) of
				{PowerStrip_SerialId, ReqestPid} ->
					%%io:fwrite("Ack Checkout success:"++PowerStrip_SerialId++"\n"),
					ReqestPid ! ok,
					ack_list(lists:delete({PowerStrip_SerialId,ReqestPid}, Data));
				false ->
					%%io:fwrite("Ack Checkout failed:"++PowerStrip_SerialId++"\n"),
					ack_list(Data)
			end
	end.		

%% @doc
%% Waits for messages and relays them to the right process.
%% @end
mailbox(Input, Ack_List) ->
	receive
		{new, {PowerStrip_SerialId, Socket}} ->
			Input ! {new, {PowerStrip_SerialId, Socket}},
			mailbox(Input, Ack_List);
		{remove, {Socket}} ->
			Input ! {remove, {Socket}},
			mailbox(Input, Ack_List );
		{send, {PowerStrip_SerialId, Status, RequestSocket}} ->
			spawn(?MODULE, send, [PowerStrip_SerialId, Status, RequestSocket, Input, Ack_List]),
			mailbox(Input, Ack_List );
		{ack, PowerStrip_SerialId} ->
			io:fwrite(PowerStrip_SerialId++"\n"),
			Ack_List ! {ack_checkout, PowerStrip_SerialId},
			mailbox(Input, Ack_List );
		{'DOWN' ,_Ref ,process ,Input ,_Reason} ->
			New_Input = spawn_link(?MODULE, input ,[]),
			mailbox(New_Input, Ack_List);
		{'DOWN' ,_Ref ,process ,Ack_List ,_Reason} ->
			New_Ack_List = spawn_link(?MODULE, ack_list ,[]),
			mailbox(Input, New_Ack_List);
		_->
			io:fwrite("Bad Data\n"),
			mailbox(Input, Ack_List )
	end.

%% @doc
%% Sends a message to the input process with the SID and then waits for the answer.
%% @end
%% @spec (SID, Status) -> (string() | {error, Reason})
%% SID = string()
%% Status = string()
send(PowerStrip_SerialId, Status, RequestSocket, Input, Ack_List) ->
	Input ! {get_socket, {PowerStrip_SerialId, self()}},
	receive 
		{found, Socket} ->
			io:fwrite("Found\n"),
			Ack_List ! {ack_request, {PowerStrip_SerialId, self()}},
			case gen_tcp:send(Socket, PowerStrip_SerialId++":"++Status++"\n") of
				ok ->
					receive 
						ok ->
							ack_sucess(RequestSocket)
					after 
						10000 ->
							ack_failed(RequestSocket)
					end,
					PowerStrip_Id = integer_to_list(sql_builder:get_powerStripId(PowerStrip_SerialId)),
					SocketId = [integer_to_list(N) || {N} <- sql_builder:get_socketId(PowerStrip_Id)],
					Sql = sql_builder:new_status(SocketId, string:tokens(Status, ";")),
					odbc_unit ! {insert, Sql};
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
