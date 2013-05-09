
-module(powerstrip).

%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/2]).

decode([Package], Socket) ->
	io:fwrite("Package data: "++Package),
	Strip = fun(Str) -> string:sub_string(Str, 2, string:len(Str)-2) end,
	List = [{A,B} || [A,B] <- [string:tokens(A, ":") || A <- string:tokens(Strip(Package), ",")]],
	case List of
		[{"id",PowerStrip_SerialId}|Data] ->
			case Data of
				[{"status", Status}] ->
					controller ! {send,{PowerStrip_SerialId, Status, Socket}};
				[{"power",Power},{"status",Status},{"date",Date},{"time",Time}] ->	
					controller ! {new,{PowerStrip_SerialId,Socket}},
					sql_builder:input([[PowerStrip_SerialId,string:tokens(Power, ";"),string:tokens(Status, ";"),string:tokens(Date, ";"),string:tokens(Time, ";")]]),
					analyzer ! {read, PowerStrip_SerialId};
				_Else ->
					case_error(Data)
			end;
		_Else ->
			io:fwrite(List),
			case_error(Package)
	end. 
	
case_error(Data) ->
	io:fwrite("Error no matching case, tcp packet thrown away.\n"),
	io:fwrite(Data),
	io:fwrite("\n").