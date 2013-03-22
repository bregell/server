%% @author Johan
%% @doc @todo Add description to test_client.


-module(test_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,start/2,start/3,worker/3,send/4,receiver/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the test client on port 39500 on localhost 
%% @end
%% @spec (Input) -> any()
%% Input = string()
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
	case gen_tcp:connect(Address, Port, [list, {active, false}, {packet, 0}]) of
		{ok, Socket} ->
			Pid = spawn_link(?MODULE, worker, [Socket, PowerStrip_Id, ["0","0","0","0"]]),
			spawn_link(?MODULE, receiver, [Socket, Pid]),
			Pid ! start;
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
send(Msg, Socket, PowerStrip_Id, Status) ->
	random:seed(now()),
	Data = [if S=="1"-> N; true -> 0 end|| {N,S} <- lists:zip([100+random:uniform(100), 200+random:uniform(300), 400+random:uniform(200), 100+random:uniform(100)],Status)],
	Convert = fun(A) -> lists:map(fun(B) -> integer_to_list(B) end, A) end,
	Packet = fun(A) -> A++":"++string:join(Convert(Data), ";")++":"++string:join(Status, ";") end,
	case gen_tcp:send(Socket, Packet(PowerStrip_Id)) of
		ok ->
			io:fwrite("Data sent:"++Packet(PowerStrip_Id)++" \n");
		{error, Reason} ->
			io:fwrite("Error: "),
			io:fwrite(Reason),
			io:fwrite("\n")
	end,
	timer:sleep(timer:seconds(10)),
	Msg ! start.

%% @doc
%% Waits for messages on the given socket and then prints them.
%% @end
%% @spec (Socket) -> string()
%% Socket = socket()
receiver(Socket, Pid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			io:fwrite("Received: "),
			io:fwrite(Packet),
			[_,Status_list] = string:tokens(Packet, ":"),
			Status = string:tokens(Status_list, ";"),
			Pid ! {status, Status},
			io:fwrite("\n"),
			receiver(Socket, Pid);
		{error, _} ->
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
		start ->
			%% @todo Add chech to see if Socket is closed and if it is reconnect.
			spawn_link(?MODULE, send, [self(), Socket, PowerStrip_Id, Current_Status]);
		{status, Status} ->
			worker(Socket, PowerStrip_Id, [if N/="D" -> N; true -> O end || {N,O} <- lists:zip(Status, Current_Status)]);
		_ ->
			io:fwrite("Bad Msg \n")
	end,
	worker(Socket, PowerStrip_Id, Current_Status).
