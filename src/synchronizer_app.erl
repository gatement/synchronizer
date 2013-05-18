-module(synchronizer_app).
-behaviour(application).
%% application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case synchronizer_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.


stop(_State) ->
    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
