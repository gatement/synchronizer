-module(smtp_client).
-export([send_email/5,
		test/3]).

-vsn("0.1.1").


%% ===================================================================
%% API functions
%% ===================================================================


send_email(From, FromPwd, Receivers, Subject, Content) ->
	case string:to_lower(lists:nth(2,string:tokens(From, "@"))) of
		"gmail.com" ->
				SmtpUrl = "smtp.gmail.com",
				SmtpPort = 465,
				send_email(From, FromPwd, Receivers, Subject, Content, SmtpUrl, SmtpPort);
		"163.com" ->
				SmtpUrl = "smtp.163.com",
				SmtpPort = 465,
				send_email(From, FromPwd, Receivers, Subject, Content, SmtpUrl, SmtpPort);
		Mail ->
			error_logger:info_msg("================ sending email error ================~n"),
			error_logger:info_msg("Mail ~p is not supported~n", [Mail]),
			error_logger:info_msg("(~s)~n~n", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')])
	end.


send_email(From, FromPwd, Receivers, Subject, Content, SmtpUrl, SmtpPort) ->
	error_logger:info_msg("================ start sending email ================~n"),

	application:start(crypto),
	application:start(public_key),
	application:start(ssl),

	case ssl:connect(SmtpUrl, SmtpPort, [{active, false}], 60000) of
	    {ok, Socket} ->
		    recv(Socket),
		    send(Socket, "HELO johnsonlau.net"),
		    send(Socket, "AUTH LOGIN"),
		    send(Socket, binary_to_list(base64:encode(From))),
		    send(Socket, binary_to_list(base64:encode(FromPwd))),
		    send(Socket, "MAIL FROM: <" ++ From ++ ">"),
		    lists:foreach(fun(Mail) -> send(Socket, "RCPT TO:<" ++ Mail ++ ">") end, Receivers),
		    send(Socket, "DATA"),
		    send_no_receive(Socket, "MIME-Version: 1.0"),
		    send_no_receive(Socket, "Content-Type: text/html; charset=UTF-8"),
		    send_no_receive(Socket, "From: <" ++ From ++ ">"),
		    lists:foreach(fun(Mail) -> send_no_receive(Socket, "To: <" ++ Mail ++ ">") end, Receivers),
		    send_no_receive(Socket, "Date: " ++ get_time()),
		    send_no_receive(Socket, "Subject: " ++ Subject),
		    send_no_receive(Socket, ""),
		    send_no_receive(Socket, Content),
		    send_no_receive(Socket, ""),
		    send(Socket, "."),
		    send(Socket, "QUIT"),
		    ssl:close(Socket),

			error_logger:info_msg("================ end sending email ================~n"),
			error_logger:info_msg("(~s)~n~n", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),

			ok;

		{error, Reason} -> 
			error_logger:info_msg("Mail error: ~p~n", [Reason]),
			error_logger:info_msg("================ end sending email ================~n"),
			error_logger:info_msg("(~s)~n~n", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),

			erlang:throw(io_lib:format("Mail error: ~p", [Reason])),

			error
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================

get_time() -> 
	{{Year,Month,Day},{Hour,Min,Sec}} = erlang:universaltime(),
	io_lib:format("~p-~p-~p ~p:~p:~p +0000", [Year,Month,Day,Hour,Min,Sec]).


send_no_receive(Socket, Data) ->
	error_logger:info_msg("Mail C: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
	error_logger:info_msg("Mail C: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).


recv(Socket) ->
    case ssl:recv(Socket, 0, 60000) of
		{ok, _Return} -> 
			error_logger:info_msg("Mail S: ~p~n", [_Return]),
			ok;
		{error, _Reason} -> 
			error_logger:info_msg("Mail error: ~p~n", [_Reason]),
			error
    end.


%% ===================================================================
%% Test Functions
%% ===================================================================

test(From, FromPwd, Receivers) ->
	Subject = io_lib:format("Gmail Test Email (~s)", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),
	Body = "<html><body><table border=\"1\"  cellspacing=\"0\">"
		"<tr><td><strong>Time</strong></td><td><strong>Bytes</strong></td><td><strong>Count</strong></td><td><strong>From</strong></td><td><strong>To</strong></td><td><strong>Loss</strong></td><td><strong>Time</strong></td></tr>"
		"<tr><td>2012-4-12 13:45:23</td><td>64</td><td>12</td><td>192.168.1.2</td><td>192.168.35.35 (Dev)</td><td style=\"color: #FF0000;\"><strong>23%</strong></td><td style=\"color: #FF0000;\"><strong>545ms</strong></td></tr>"
		"<tr><td>2012-4-12 13:45:23</td><td>64</td><td>12</td><td>192.168.1.2</td><td>192.168.35.35 (Dev)</td><td>2%</td><td>345ms</td></tr>"
		"</table></body></html>",

	send_email(From, FromPwd, Receivers, Subject, Body).
