-module(notification).
-export([send/2]).


%% ===================================================================
%% application callbacks
%% ===================================================================

send(Msg, Persistence) ->
	application:start(crypto),
	inets:start(),
	ssl:start(),
	
	case get_session_id() of
		false -> error;
		SessionId ->
			%io:format("session id: ~p~n", [SessionId]),

			{ok, Url} = case Persistence of
				true -> application:get_env(url_send_persistence_msg);
				false -> application:get_env(url_send_msg)
			end,

			%% append time to the msg
			Msg2 = lists:flatten(io_lib:format("~s [~s]", [Msg, tools:datetime_string('hh:mm')])),

			Msg3 = edoc_lib:escape_uri(Msg2),
			Body = lists:concat(["msg=", Msg3]),
			Cookie = io_lib:format("usr_sid=~s", [SessionId]),
			Request = {Url, [{"Cookie", Cookie}], "application/x-www-form-urlencoded", Body},

			_Result = httpc:request(post, Request, [], []),
			%io:format("return content: ~p~n", [_Result]),
			ok
	end,

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================

get_session_id() ->
	{ok, Url} = application:get_env(url_user_session),
	{ok, UserId} = application:get_env(mqtt_user),
	{ok, Password} = application:get_env(mqtt_user_pwd),

	UserId2 = edoc_lib:escape_uri(UserId),
	Password2 = edoc_lib:escape_uri(Password),
	Body = lists:concat(["id=", UserId2, "&pwd=", Password2]),
	Request = {Url, [], "application/x-www-form-urlencoded", Body},

	{Code, Result} = httpc:request(post, Request, [], []),
	case Code of
		error ->
			false;
		ok ->
			{_, _, Content} = Result,
			{ok, {struct, Results}} = json2:decode_string(Content),
			ReturnCode = proplists:get_value("success", Results),
			case ReturnCode of
				false -> false;
				true -> proplists:get_value("data", Results)
			end
	end.

