%% @author Johan 
%% @doc Will build the commands that will be sent to the
%% controller to send the signals to the units.

-module(command_builder). 

%% ====================================================================
%% API functions
%% ====================================================================

-export([input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

input(ResultTuple) ->
	io:fwrite(ResultTuple).
