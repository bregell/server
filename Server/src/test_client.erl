%% @author Johan
%% @doc @todo Add description to test_client.


-module(test_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,start/1,start/2,start/3,stress_test/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================
-export([receiver/4,send/4]).

%% @doc
%% Starts the test client on port 39500 on localhost 
%% @end
%% @spec (Input) -> any()
%% Input = string()
stress_test(Total,Current) ->
	case Current of
		Total ->
			start("bregell.mine.nu", 39500, "SN-TEST"++integer_to_list(Current));
		_Else ->
			spawn(?MODULE, start, ["bregell.mine.nu", 39500, "SN-TEST"++integer_to_list(Current)]),
			timer:sleep(timer:seconds(1)),
			stress_test(Total, Current+1)
	end.
	
%%start() ->
	%%spawn(?MODULE, start, ["bregell.mine.nu", 39500, "SN-ANDRO1"]),
	%%spawn(?MODULE, start, ["bregell.mine.nu", 39500, "SN-ANDRO2"]).
	
start() ->
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "Bregell"]),
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "Lagerman"]),
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "Phan"]),
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "Arvidsson"]),
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "Swetzen"]).
	
start(PowerStrip_Id) ->
	start("bregell.mine.nu", 39500, PowerStrip_Id).

%% @doc
%% Same as start/1 but alsp specify adress
%% @end
%% @spec (Address,Input) -> any()
%% Adress = (inet:address() | inet:hostname())
%% Input = string()
start(Adress, PowerStrip_Id) ->
	start(Adress, 39500, PowerStrip_Id).

%% @doc
%% Same as start/2 but also specify port
%% @end
%% @spec (Address,Port,Input) -> any()
%% Adress = (inet:address() | inet:hostname())
%% Port = inet:portnumber()
%% Input = string()
start(Address, Port, PowerStrip_Id) ->
	case gen_tcp:connect(Address, Port, [list, {active, true}, {packet, line}]) of
		{ok, Socket} ->
			worker(Socket, PowerStrip_Id, ["1","1","1","1"]);
		{error, Reason} ->
			io:fwrite("Error: "),
			io:fwrite(Reason)
	end.

%% @doc
%% Sends mockupdata to the server
%% @end
%% @spec (Msg, Socket, Units) -> string()
%% Msg = pid()
%% Socket = socket()
%% Units = [UnitID] 
%% UnitID = string()
send(Socket, PowerStrip_Id, Status, Pid) ->
	%% Create random data
	random:seed(now()),
	Data = [if S=="1"-> N; true -> 0 end|| {N,S} <- lists:zip([100+random:uniform(50), 200+random:uniform(100), 300+random:uniform(150), 400+random:uniform(50)],Status)],
	Convert = fun(A) -> lists:map(fun(B) -> integer_to_list(B) end, A) end,
	
	%% Create time
	Timestamp = calendar:now_to_datetime(erlang:now()),
	Date = string:join([integer_to_list(N) || N <- [element(1,element(1,Timestamp)),element(2,element(1,Timestamp)),element(3,element(1,Timestamp))]], ";"),
	Time = string:join([integer_to_list(N) || N <- [element(1,element(2,Timestamp)),element(2,element(2,Timestamp)),element(3,element(2,Timestamp))]], ";"),
	
	%% Create packet
	Packet = fun(A) -> A++":"++string:join(Convert(Data), ";")++":"++string:join(Status, ";")++":"++Date++":"++Time++"\n" end,
	case gen_tcp:send(Socket, Packet(PowerStrip_Id)) of
		ok ->
			io:fwrite("Data sent:"++Packet(PowerStrip_Id));
		{error, Reason} ->
			Pid ! socket_closed,
			io:fwrite("Error: "),
			io:fwrite(Reason),
			io:fwrite("\n")
	end.

%% @doc
%% Waits for messages on the given socket and then prints them.
%% @end
%% @spec (Socket) -> string()
%% Socket = socket()
% =ERROR REPORT==== 24-May-2013::21:10:17 ===
% Error in process <0.396.0> with exit value: {undef,[{test_client,reciever,["SN-ANDRO1:0;D;D;D\n",#Port<0.2442>,<0.32.0>,"SN-ANDRO1"],[]}]}

receiver(Packet, Socket, Pid, PowerStrip_Id) ->
	Ack_Packet = PowerStrip_Id++":OK\n",
	io:fwrite(Ack_Packet),
	case gen_tcp:send(Socket, Ack_Packet) of
		ok ->
			Strip = fun(A) -> string:sub_string(A, 1, string:len(A)-1) end,
			Data = Strip(Packet),
			io:fwrite("Received:"++Data++"\n"),
			[_,Status_list] = string:tokens(Data, ":"),
			Status = string:tokens(Status_list, ";"),
			Pid ! {status, Status};
		{error, _} ->
			Pid ! socket_closed,
			io:fwrite("Could not receive\n")
	end.
		

%% @doc
%% Waits for a message to start a new instance of send
%% @end
%% @spec (Socket, Units) -> string()
%% Socket = socket()
%% Units = [UnitID] 
%% UnitID = string()
worker(Socket, PowerStrip_Id, Current_Status) -> 
	receive
		{status, Status} ->
			worker(Socket, PowerStrip_Id, [if N/="D" -> N; true -> O end || {N,O} <- lists:zip(Status, Current_Status)]);
		socket_closed ->
			io:fwrite("Conection lost\n");
		{tcp, _Socket, Packet} ->
			spawn(?MODULE, receiver, [Packet, Socket, self(), PowerStrip_Id]),
			worker(Socket, PowerStrip_Id, Current_Status);
		_ ->
			io:fwrite("Bad Msg\n"),
			worker(Socket, PowerStrip_Id, Current_Status)
	after
		10000 ->
			spawn(?MODULE, send, [Socket, PowerStrip_Id, Current_Status, self()]),
			worker(Socket, PowerStrip_Id, Current_Status)
	end.
