-module(ssl_server). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([server/1, listener/1]).

server(Port) ->
	ssl:start(),
	case gen_tcp:listen(Port, [list, {active, false}, {packet, line}]) of
		{ok, ListenSocket} ->
			spawn(?MODULE, listener, [ListenSocket]),
			loop();
		{error, Reason} ->
			io:fwrite("Error cannot listen on that port \n"),  
			io:fwrite(Reason),
			io:fwrite("\n")
	end.
	
listener(ListenSocket) ->
	io:fwrite("Waiting for connection\n"),
	case tcp_setup(ListenSocket) of
		{ok, Socket} ->
			spawn(?MODULE, listener, [ListenSocket]),
			case ssl_setup(Socket) of
				{ok, SSLSocket} ->
					ssl_receiver(SSLSocket);
				{error, Reason} ->
					receiver(Socket)
			end;
		{error, Reason} ->
			spawn(?MODULE, listener, [ListenSocket]),
			io:fwrite(Reason)
	end.

ssl_setup(Socket) ->
	case ssl:ssl_accept(
		Socket, 
		[
			{certfile, "cert.pem"}, 
			{keyfile, "key.pem"}
		],
		infinity
	) of 
		{ok, SSLSocket} ->
			{ok, SSLSocket};
		{error, Reason} ->
			{error, Reason}
	end.
	
tcp_setup(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			{ok, Socket};
		{error, Reason} ->
			{error, Reason}
	end.

receiver(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Package} ->
			io:fwrite(Package),
			receiver(Socket);
		{error, closed} ->
			io:fwrite("Closed TCP\n");
		{error, Reason} ->
			io:fwrite(Reason),
			receiver(Socket)
	end.
	
ssl_receiver(SSLSocket) ->
	case ssl:recv(SSLSocket, 0) of
		{ok, Package} ->
			io:fwrite(Package),
			ssl_receiver(SSLSocket);
		{error, closed} ->
			io:fwrite("Closed SSL\n");
		{error, Reason} ->
			io:fwrite(Reason),
			ssl_receiver(SSLSocket)
	end.
	
loop() ->
	receive
		_Else ->
			flush	
	end,
	loop().