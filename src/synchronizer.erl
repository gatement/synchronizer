-module(synchronizer).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->
    ssh:start(),
	application:start(synchronizer).


%% ===================================================================
%% Local Functions
%% ===================================================================