%% @author Johan 
%% @doc Takes input from the listener and asks the 
%% database for the history of that unit.


-module(analyzer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

input(Input) ->
	case Input of
		[Unit, Data, Status] ->
			io:fwrite(Unit),
			io:fwrite("\n"),
			io:fwrite(Data),
			io:fwrite("\n"),
			io:fwrite(Status),
			io:fwrite("\n")
	end.		
