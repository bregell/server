%% @author Johan
%% @doc @todo Add description to test_client.


-module(test_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,start/2,start/3,worker/2,send/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Starts the test client on port 39500 on localhost 
%% @end
%% @spec (Input) -> any()
%% Input = string()
start(Input) ->
	start(localhost, 39500, Input).

%% @doc
%% Same as start/1 but alsp specify adress
%% @end
%% @spec (Address,Input) -> any()
%% Adress = (inet:address() | inet:hostname())
%% Input = string()
start(Adress, Input) ->
	start(Adress, 39500, Input).

%% @doc
%% Same as start/2 but also specify port
%% @end
%% @spec (Address,Port,Input) -> any()
%% Adress = (inet:address() | inet:hostname())
%% Port = inet:portnumber()
%% Input = string()
start(Address, Port, Input) ->
	case gen_tcp:connect(Address, Port, [list, {active, false}, {packet, 0}]) of
		{ok, Socket} ->
			Pid = spawn_link(?MODULE, worker, [Socket, string:tokens(Input, ":")]),
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
send(Msg, Socket, Units) ->
	timer:sleep(timer:seconds(10)),
	Msg ! start,
	random:seed(now()),
	Data = [random:uniform(500), random:uniform(500), random:uniform(500), random:uniform(500)],
	Convert = fun(A) -> lists:map(fun(B) -> integer_to_list(B) end, A) end,
	Packet = fun(A) -> A++":"++string:join(Convert(Data), ";")++":1;1;1;1" end,
	case lists:foreach(fun(A) -> gen_tcp:send(Socket, Packet(A)) end, Units) of
		ok ->
			io:fwrite("Data sent \n");
		{error, Reason} ->
			io:fwrite("Error: "),
			io:fwrite(Reason),
			io:fwrite("\n")
	end.

%% @doc
%% Waits for a message to start a new instance of send
%% @end
%% @spec (Socket, Units) -> string()
%% Socket = socket()
%% Units = [UnitID] 
%% UnitID = string()
worker(Socket, Units) -> 
	receive
		start ->
			spawn_link(?MODULE, send, [self(), Socket, Units]);
		_ ->
			io:fwrite("Bad Msg \n")
	end,
	worker(Socket, Units).
