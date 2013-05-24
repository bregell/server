%% @author Johan
%% @doc @todo Add description to test_client.


-module(test_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,start/1,start/2,start/3,test/2, worker/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the test client on port 39500 on localhost 
%% @end
%% @spec (Input) -> any()
%% Input = string()
test(Total,Current) ->
	case Current of
		Total ->
			start("bregell.mine.nu", 39500, "SN-TEST"++integer_to_list(Current));
		_Else ->
			spawn(?MODULE, start, ["bregell.mine.nu", 39500, "SN-TEST"++integer_to_list(Current)]),
			timer:sleep(timer:seconds(1)),
			test(Total, Current+1)
	end.
	
start() ->
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "SN-ANDRO1"]),
	spawn(?MODULE, start, ["bregell.mine.nu", 39500, "SN-ANDRO2"]).
	
start(PowerStrip_Id) ->
	start(localhost, 39500, PowerStrip_Id).

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
	case gen_tcp:connect(Address, Port, [list, {active, false}, {packet, line}]) of
		{ok, Socket} ->
			Pid = spawn_link(?MODULE, worker, [Socket, PowerStrip_Id, ["1","1","1","1"]]),
			receiver(Socket, Pid, PowerStrip_Id);
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
send(Socket, PowerStrip_Id, Status) ->
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
			io:fwrite("Error: "),
			io:fwrite(Reason),
			io:fwrite("\n")
	end.

%% @doc
%% Waits for messages on the given socket and then prints them.
%% @end
%% @spec (Socket) -> string()
%% Socket = socket()
receiver(Socket, Pid, PowerStrip_Id) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			case gen_tcp:send(Socket, PowerStrip_Id++":OK\n") of
				ok ->
					io:fwrite("Received: "),
					io:fwrite(Packet),
					[_,Status_list] = string:tokens(Packet, ":"),
					Status = string:tokens(Status_list, ";"),
					Pid ! {status, Status},
					io:fwrite("\n"),
					receiver(Socket, Pid, PowerStrip_Id);
				{error, _} ->
					Pid ! socket_closed,
					io:fwrite("Could not receive\n")
			end;
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
			io:fwrite("Conection lost");
		_ ->
			io:fwrite("Bad Msg \n"),
			worker(Socket, PowerStrip_Id, Current_Status)
	after
		10000 ->
			send(Socket, PowerStrip_Id, Current_Status),
			worker(Socket, PowerStrip_Id, Current_Status)
	end.
