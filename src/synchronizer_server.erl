-module(synchronizer_server).
-include_lib("kernel/include/file.hrl").
-behaviour(gen_server).
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {run_interval, error_interval}).
-define(SSH_RETRY_TIMES, 3).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, WarmingTime} = application:get_env(wait_before_first_run),
	{ok, ErrorInterval} = application:get_env(wait_after_error_occur),
	{ok, RunInterval} = application:get_env(run_interval),

    State = #state{
    	run_interval = RunInterval * 1000, 
    	error_interval = ErrorInterval * 1000
	},

    {ok, State, WarmingTime * 1000}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, #state{ run_interval = RunInterval, error_interval = ErrorInterval} = State) ->
	case sync() of
		ok -> 
            {ok, ExitOnSuccess} = application:get_env(exit_on_success),
            case ExitOnSuccess of
                true ->
                    {stop, normal, State};
                _ ->
                    {noreply, State, RunInterval}
            end;
		error -> 
            {noreply, State, ErrorInterval}
	end;
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Local Functions
%% ===================================================================

sync() ->
	{ok, RemoteFolder} = application:get_env(remote_folder),
	{ok, LocalFolder} = application:get_env(local_folder),
	{ok, KeepLocalFiles} = application:get_env(keep_local_files),
	{ok, Server} = application:get_env(ssh_server),

	erlang:put(error, false),

	output_message("======== start synchronizing ========~n", []),
	output_message("From = ~s:~s~n", [Server, RemoteFolder]),
	output_message("To = ~s~n", [LocalFolder]),

	Result = try
		{ok, ChannelPid, ConnectionRef} = open_ssh_channel(),
		erlang:put(ssh_channel, {ChannelPid, ConnectionRef}),

		sync_folder(LocalFolder, RemoteFolder, KeepLocalFiles),

		case erlang:get(error) of
			false ->
				output_message("========> success~n~n", []);
			true->
				output_message("========> success(partial)~n~n", [])
		end,
		send_success_email(),
		ok
	catch
		_:Reason ->
			output_message("~n========> error~n", []),
			output_message("error ==>~p~n", [Reason]),
			send_error_email(Reason),
			error
	end,

	{ChannelPid2, ConnectionRef2} = erlang:get(ssh_channel),
	ssh_sftp:stop_channel(ChannelPid2),
	ssh:close(ConnectionRef2),

	Result.


sync_folder(LocalFolder, RemoteFolder, KeepLocalFiles) ->
	{ChannelPid, _} = erlang:get(ssh_channel),

	% create local folder if need
	ok = filelib:ensure_dir(LocalFolder ++ "/"),

	{ok, RemoteFileNames} = ssh_sftp:list_dir(ChannelPid, RemoteFolder),

	% delete local items if not exist
	if 
		KeepLocalFiles =:= false -> 
			purge_not_in_list_local_items(LocalFolder, RemoteFileNames);
		true -> 
			do_nothing
	end,

	Fun = fun(FileName) -> 
		if 
			FileName =:= "." -> 
				ignore;
			FileName =:= ".." -> 
				ignore;
			true ->
				RemoteFile = RemoteFolder ++ "/" ++ FileName,
				case ssh_read_file_info(RemoteFile, ?SSH_RETRY_TIMES) of
					{ok, RemoteFileInfo} ->
						case RemoteFileInfo#file_info.type of
							regular -> copy_file_if_modified(LocalFolder ++ "/" ++ FileName, RemoteFile, RemoteFileInfo);
							directory -> sync_folder(LocalFolder ++ "/" ++ FileName, RemoteFile, KeepLocalFiles);
							_ -> ignore
						end;
					unexcepted_value ->
						erlang:put(error, true)
				end
		end
	end,

	lists:foreach(Fun, RemoteFileNames).


purge_not_in_list_local_items(LocalFolder, RemoteFileNames) ->
	{ok, LocalFileNames} = file:list_dir(LocalFolder),

	Fun = fun(FileName) ->
		if 
			FileName =:= "." -> ignore;
			FileName =:= ".." -> ignore;
			true ->
			 	IsInList = lists:member(FileName, RemoteFileNames),
			 	case IsInList of
			 		true -> do_nothing;
			 		false -> delete_local_item_recursively(LocalFolder ++ "/" ++ FileName)
			 	end
		end
	end,

	lists:foreach(Fun, LocalFileNames).


delete_local_item_recursively(LocalFile) ->
	case filelib:is_dir(LocalFile) of
		false ->
			file:delete(LocalFile),
			output_message("deleted local file: ~s~n", [LocalFile]);

		true -> 
			{ok, FileNames} = file:list_dir(LocalFile),

			Fun = fun(FileName) ->
				if 
					FileName =:= "." -> ignore;
					FileName =:= ".." -> ignore;
					true -> delete_local_item_recursively(LocalFile ++ "/" ++ FileName)
				end
			end,

			% delete sub-items
			lists:foreach(Fun, FileNames),

			% delete folder itself
			file:del_dir(LocalFile),
			output_message("deleted local folder: ~s~n", [LocalFile])
	end.


copy_file_if_modified(LocalFile, RemoteFile, RemoteFileInfo) ->
	case filelib:is_regular(LocalFile) of
		false ->
			% no local file
			copy_file(LocalFile, RemoteFile);

		true ->
			LocalLastModified = filelib:last_modified(LocalFile),
			RemoteLastModified = RemoteFileInfo#file_info.mtime,
			if
				RemoteLastModified > LocalLastModified -> copy_file(LocalFile, RemoteFile);
				true -> output_message("skipped: ~s~n", [RemoteFile], debug)
			end
	end.


copy_file(LocalFile, RemoteFile) ->
	{ChannelPid, _} = erlang:get(ssh_channel),

	output_message("copying: ~s ", [RemoteFile]),
	{ok, FileData} = ssh_sftp:read_file(ChannelPid, RemoteFile),
	file:write_file(LocalFile, FileData),
	output_message("[done]~n", []).


send_error_email(Error) ->
	{ok, ClientName} = application:get_env(client_name),
	{ok, SenderEmail} = application:get_env(sender_email),
	{ok, SenderEmailPwd} = application:get_env(sender_email_password),
	{ok, ReceiverEmail} = application:get_env(receiver_email),
	Subject = io_lib:format("Sync Error [~s] (~s)", [ClientName, tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),
	Content = io_lib:format("~p", [Error]),
	smtp_client:send_email(SenderEmail, SenderEmailPwd, [ReceiverEmail], Subject, Content).


send_success_email() ->
	{ok, ClientName} = application:get_env(client_name),
	{ok, SenderEmail} = application:get_env(sender_email),
	{ok, SenderEmailPwd} = application:get_env(sender_email_password),
	{ok, ReceiverEmail} = application:get_env(receiver_email),
	Subject = io_lib:format("Sync Success [~s] (~s)", [ClientName, tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),
	Content = case erlang:get(error) of
		false -> "Sync Success.";
		true -> "Sync Partial Success."
	end,
	smtp_client:send_email(SenderEmail, SenderEmailPwd, [ReceiverEmail], Subject, Content).


ssh_read_file_info(RemoteFile, 0) ->
	output_message("ssh_read_file_info error(retryed ~p times and gave up), filename: ~s~n", [?SSH_RETRY_TIMES, RemoteFile]),
	ssh_read_file_info_error;

ssh_read_file_info(RemoteFile, RetryTimes) ->
	{ChannelPid, ConnectionRef} = erlang:get(ssh_channel),

	case ssh_sftp:read_file_info(ChannelPid, RemoteFile) of
		{ok, RemoteFileInfo} -> 
			{ok, RemoteFileInfo};

		{error, closed} ->
			output_message("ssh_read_file_info error(retry ~p times), msg: ~p, filename: ~s~n", [?SSH_RETRY_TIMES - RetryTimes + 1, closed, RemoteFile]),

			ssh_sftp:stop_channel(ChannelPid),
			ssh:close(ConnectionRef),

			timer:sleep(2000),

			{ok, ChannelPid2, ConnectionRef2} = open_ssh_channel(),
			erlang:put(ssh_channel, {ChannelPid2, ConnectionRef2}),

			ssh_read_file_info(RemoteFile, RetryTimes - 1);

		Msg -> 
			output_message("ssh_read_file_info error, msg: ~p, filename: ~s~n", [Msg, RemoteFile]),
			Msg
	end.


open_ssh_channel() ->
	{ok, Server} = application:get_env(ssh_server),
	{ok, Port} = application:get_env(ssh_port),
	{ok, User} = application:get_env(ssh_user),
	{ok, Pwd} = application:get_env(ssh_password),

	{ok, ChannelPid, ConnectionRef} = ssh_sftp:start_channel(Server, Port, [{user, User}, {password, Pwd}]),
	{ok, ChannelPid, ConnectionRef}.


output_message(Format, Params) ->
	output_message(Format, Params, info).


output_message(Format, Params, Level) ->
	{ok, OutputDebug} = application:get_env(output_debug),
	case Level of
		debug ->
			if
				OutputDebug =:= true -> 
					error_logger:info_msg(Format, Params);
				true -> 
					ignore
			end;
		_ ->
			error_logger:info_msg(Format, Params)
	end.

