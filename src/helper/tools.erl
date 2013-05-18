-module(tools).
-export([sleep/1,
		list_string_to_string/3,
		epoch_seconds/0,
		epoch_milli_seconds/0,
		epoch_macro_seconds/0,
		datetime_string/1,
		datetime_string/2,
		get_date/1,
		get_date/2,
		get_datetime/1,
		get_datetime/2,
		get_universal_date/0,
		random_string/1,
		generate_id/1,		
		record_to_list/2,
		is_pid_alive/1]).
		
-vsn("0.1.1").

%% ===================================================================
%% API functions
%% ===================================================================

sleep(Milliseconds) -> 
	receive
	after Milliseconds -> ok
	end.

	
list_string_to_string([], _, Acc) ->
	lists:flatten(Acc);
list_string_to_string([H|T], Separator, []) ->
	list_string_to_string(T, Separator, H);
list_string_to_string([H|T], Separator, Acc) ->
	list_string_to_string(T, Separator, [Acc ++ Separator ++ H]). 


epoch_seconds() ->
	{A, B, C} = erlang:now(),
	erlang:round(A*1000000 + B + C/1000000).

	
%% The same as Java DateTime.getUtcMilliSecondTimestamp()
epoch_milli_seconds() ->
	{A, B, C} = erlang:now(),
	erlang:round(A*1000000000 + B*1000 + C/1000).

	
epoch_macro_seconds() ->
	{A, B, C} = erlang:now(),
	erlang:round(A*1000000000000 + B*1000000 + C).
	

datetime_string(Format) ->
	Date = erlang:date(),
	Time = erlang:time(),
	datetime_string(Format, {Date, Time}).


datetime_string(Format, {{Year, Month, Day}, {Hour, Minute, Second}}) ->
	MonthText = if
		Month < 10 -> "0" ++ erlang:integer_to_list(Month);
		true -> erlang:integer_to_list(Month)
	end,
	DayText = if
		Day < 10 -> "0" ++ erlang:integer_to_list(Day);
		true -> erlang:integer_to_list(Day)
	end,
	HourText = if
		Hour < 10 -> "0" ++ erlang:integer_to_list(Hour);
		true -> erlang:integer_to_list(Hour)
	end,
	MinuteText = if
		Minute < 10 -> "0" ++ erlang:integer_to_list(Minute);
		true -> erlang:integer_to_list(Minute)
	end,
	SecondText = if
		Second < 10 -> "0" ++ erlang:integer_to_list(Second);
		true -> erlang:integer_to_list(Second)
	end,

	Result = case Format of
		'yyyyMMdd hh:mm' ->
			erlang:integer_to_list(Year) ++ MonthText ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText;
		'yyyyMMdd hh:mm:ss' ->
			erlang:integer_to_list(Year) ++ MonthText ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText ++ ":" ++ SecondText;
		'yyyy-MM-dd hh:mm:ss' ->
			erlang:integer_to_list(Year) ++ "-" ++ MonthText ++ "-" ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText ++ ":" ++ SecondText;
		'yyyyMMdd_hhmmss' ->
			erlang:integer_to_list(Year) ++ MonthText ++ DayText ++ "_" ++ HourText ++ MinuteText ++ SecondText;
		'yyyyMMdd' ->
			erlang:integer_to_list(Year) ++ MonthText ++ DayText
	end,

	lists:flatten(Result).
	

get_date(Days) ->
	Date = erlang:date(),
	get_date(Days, Date).

	
%% Date is {Year, Month, Day}
get_date(Days, Date) ->
	GregorianDays = calendar:date_to_gregorian_days(Date),
	calendar:gregorian_days_to_date(GregorianDays + Days).
	

get_datetime(Seconds) ->
	Date = erlang:date(),
	Time = erlang:time(),
	get_datetime(Seconds, {Date, Time}).


get_datetime(Seconds, Datetime) ->
	GregorianSeconds = calendar:datetime_to_gregorian_seconds(Datetime),
	calendar:gregorian_seconds_to_datetime(GregorianSeconds + Seconds).

	
get_universal_date() ->
	{Date, _} = calendar:universal_time(),
	Date.
	

random_string(Len) ->
    Chrs = erlang:list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = erlang:size(Chrs),
    F = fun(_, R) -> [erlang:element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).
	
	
generate_id(Prefix) ->
	{A, B, C} = erlang:now(),
	Id = Prefix ++ erlang:integer_to_list(A) ++ erlang:integer_to_list(B) ++ erlang:integer_to_list(C),
	lists:flatten(Id).


record_to_list(Record, Attributes) ->
	Fun = fun(X, Acc) -> 
		[{erlang:atom_to_list(X), erlang:element(erlang:length(Acc) + 2, Record)} | Acc] 
	end,
	lists:foldl(Fun, [], Attributes).


is_pid_alive(Pid) when node(Pid) =:= node() ->
    erlang:is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
		false ->
		    false;
		true ->
		    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
				true ->
				    true;
				false ->
				    false;
				{badrpc, _Reason} ->
				    false
		    end
    end.

    
%% ===================================================================
%% Local Functions
%% ===================================================================
