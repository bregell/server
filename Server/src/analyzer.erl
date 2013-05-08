%% @author Johan 
%% @doc Takes input from the listener and asks the 
%% database for the history of that unit.
%% @todo EDoc comment the module.
%% @todo Something is wrong and it throws an error somewhere.

-module(analyzer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, worker/2, analyzer/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
	mailbox().

mailbox() ->
	receive
		{read, PowerStrip_SerialId} ->
			spawn(?MODULE, worker, [PowerStrip_SerialId, 24])
	end,
	mailbox().

worker(PowerStrip_SerialId, Length) ->
	%% Get history
	Result = sql_builder:get_history(PowerStrip_SerialId, Length),
	%% Get current status
	Status_current = sql_builder:get_status(PowerStrip_SerialId),
	%% Convert status to string
	Status_now = [integer_to_list(N) || {N} <- Status_current],
	Status_bools = analyzer(Result),
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
	case Status_out of
		["D","D","D","D"] ->
			io:fwrite("D"),
			ok;
		_Else ->
			controller ! {send,{PowerStrip_SerialId, string:join(Status_out, ";")}}
	end.

analyzer(Answer) ->
	{Sub_List1, Sub_List2} = lists:split(length(Answer) div 2, Answer),
	{List1, List2} = lists:split(length(Sub_List1) div 2, Sub_List1),
	{List3, List4} = lists:split(length(Sub_List2) div 2, Sub_List2),
	List = [List1,List2,List3,List4],
	[lists:member(true, [Y >= 5 || {_,Y} <- N]) || N <- List].
