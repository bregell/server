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
		{read, SID} ->
			spawn_link(?MODULE, worker, [SID, 24])
	end,
	mailbox().

worker(SID, Length) ->
	try sql_builder:input({SID, Length}) of
		{ok,[Answer]} ->
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
			Status = [Bool_to_int(N) || N <- Status_bools],
			controller ! {send,{SID, lists:reverse(string:join(Status, ";"))}}
	catch 
		{error,_} ->
			io:fwrite("Error when sending to SQL_builder\n");
		_ -> 
			io:fwrite("Strange data from SQL Module\n")
	end.


analyzer(Answer) ->
	{selected,_,Data} = Answer,
	[Sorted] = split(lists:keysort(2,Data), [], length(Data)),
	Function = fun(B) -> B >= 200 end,
	[lists:member(true, [Function(Y) || {_,_,_,Y} <- N]) || N <- Sorted].

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

