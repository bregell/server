%% @author Johan
%% @doc This module will send out control signals to the different units,
%% Will need a way to acess the sockets from the listener to 
%% then send the data to the correct unit. 

-module(controller).

%% ====================================================================
%% API functions
%% ====================================================================
-export([input/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% Input = [Unit:Status] where Status = [A,B,C,D]
input(Input) ->
	io:fwrite(Input).
	 