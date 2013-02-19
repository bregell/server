%% @author Johan
%% @doc @todo Add description to test_client.


-module(test_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,start/2,start/3,worker/2,send/4]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Input) ->
	start(localhost, 50000, Input).
start(Port, Input) ->
	start(localhost, Port, Input).
start(Address, Port, Input) ->
	case gen_tcp:connect(Address, Port, [list, {active, false}, {packet, 0}]) of
		{ok, Socket} ->
			Pid = spawn_link(?MODULE, worker, [Socket, string:tokens(Input, ":")]),
			Pid ! start;
		{error, Reason} ->
			io:fwrite("Error: "),
			io:fwrite(Reason)
	end.

%% @todo Implement connection with database and command builder
send(Msg, Socket, Units, State) ->
	timer:sleep(timer:seconds(10)),
	Msg ! start,
	{V1,S1} = random:uniform_s(500, State),
	{V2,S2} = random:uniform_s(500, S1),
	{V3,S3} = random:uniform_s(500, S2),
	{V4,_} = random:uniform_s(500, S3),
	Data = [V1,V2,V3,V4],
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

worker(Socket, Units) -> 
	receive
		start ->
			{A1,A2,A3} = now(),
			spawn_link(?MODULE, send, [self(), Socket, Units, random:seed(A1,A2,A3)]);
		_ ->
			io:fwrite("Bad Msg \n")
	end,
	worker(Socket, Units).
