%% @author Johan
%% @doc @todo Add description to test_client.


-module(test_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3,worker/0,send/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Address, Port, Input) ->
	case gen_tcp:connect(Address, Port, [list, {active, false}, {packet, 0}]) of
		{ok, Socket} ->
			Pid = spawn_link(?MODULE, worker, []),
			loop(Pid, 10, Socket, string:tokens(Input, ":"));
		{error, Reason} ->
			io:fwrite("Error: "),
			io:fwrite(Reason)
	end.

%% @todo Implement connection with database and command builder
send(Socket, Units) ->
	Data = fun() -> [random:uniform(500),random:uniform(500),random:uniform(500),random:uniform(500)] end,
	Convert = fun(A) -> lists:map(fun(B) -> integer_to_list(B) end, A) end,
	Packet = fun(A) -> A++":"++string:join(Convert(Data()), ";")++":1;1;1;1" end,
	case lists:foreach(fun(A) -> gen_tcp:send(Socket, Packet(A)) end, Units) of
		ok ->
			io:fwrite("Data sent \n");
		{error, Reason} ->
			io:fwrite("Error: "),
			io:fwrite(Reason),
			io:fwrite("\n")
	end.

worker() ->
	receive
		{start, Socket, Units} ->
			spawn_link(?MODULE, send, [Socket, Units]);
		_ ->
			io:fwrite("Bad Msg \n")
	end,
	worker().

loop(Pid, T, Socket, Units) ->
	Time = timer:seconds(T),
	timer:send_after(Time, Pid, {start, Socket, Units}),
	loop(Pid,T+10, Socket, Units).
