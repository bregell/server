%% @author Johan 
%% @doc Takes input from the listener and asks the 
%% database for the history of that unit.


-module(analyzer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, worker/2, split/3, analyzer/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
	mailbox().

mailbox() ->
	receive
		{read, SID} ->
			spawn_link(?MODULE, worker, [SID, 960])
	end,
	mailbox().

worker(SID, Length) ->
	try sql_builder:input({SID, Length}) of
		{ok,[Answer]} ->
			Status = analyzer(Answer, Length),
			controller ! {send,{SID, Status}}
	catch 
		{error,_} ->
			io:fwrite("Error when sending to SQL_builder\n");
		_ -> 
			io:fwrite("Strange data from SQL Module\n")
	end.


analyzer(Answer, Length) ->
	{selected,_,Data} = Answer,
	[Sorted] = split(lists:keysort(2,Data), [], Length),
	Function = fun(B) -> B >= 5 end,
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

