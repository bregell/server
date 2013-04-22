%% @author Johan 
%% @doc Takes input from the listener and asks the 
%% database for the history of that unit.
%% @todo EDoc comment the module.
%% @todo Something is wrong and it throws an error somewhere.

-module(analyzer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, worker/2, split/3, analyzer/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
	mailbox().

mailbox() ->
	receive
		{read, PowerStrip_SerialId} ->
			spawn_link(?MODULE, worker, [PowerStrip_SerialId, 24])
	end,
	mailbox().

worker(PowerStrip_SerialId, Length) ->
	try sql_builder:input({PowerStrip_SerialId, Length}) of
		{ok,[Answer]} ->
			{ok,[{selected,_,Status_data}]} = sql_builder:get_status(PowerStrip_SerialId),
			Status_now = [integer_to_list(N) || {N} <- Status_data],
			Status_bools = analyzer(Answer),
			Bool_to_int = 
				fun(A) 
					 -> (case A of
							 true ->
								 "1";
							 false ->
								 "0"
						 end) 
				end,
			Status_calc = [Bool_to_int(N) || N <- Status_bools],
			Status_out = [if X==Y -> "D"; Y=="0"-> "D"; true -> X end || {X,Y} <- lists:zip(Status_calc, Status_now)],
			controller ! {send,{PowerStrip_SerialId, string:join(Status_out, ";")}}
	catch 
		{error,_} ->
			io:fwrite("Error when sending to SQL_builder\n");
		_ -> 
			io:fwrite("Strange data from SQL Module\n")
	end.


analyzer(Answer) ->
	{selected,_,Data} = Answer,
	[Sorted] = split(lists:keysort(2,Data), [], length(Data)),
	lists:reverse([lists:member(true, [Y >= 50 || {_,_,_,Y} <- N]) || N <- Sorted]).

split(List, [], Length) when length(List) < Length div 4 ->
	[List];
split(List, Ans, Length) when length(List) < Length div 4 ->
	case List of
		[] ->
			[Ans];
		_ ->
			[List|Ans]
	end;
split(List, [], Length) ->
	{X,Y} = lists:split(Length div 4, List),
	split(Y, [X], Length);
split(List, Ans, Length) when length(List) >= Length div 4 ->
	{X,Y} = lists:split(Length div 4, List),
	split(Y, [X|Ans], Length).
