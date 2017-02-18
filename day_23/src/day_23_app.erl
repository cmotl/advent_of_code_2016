%%%-------------------------------------------------------------------
%% @doc day_23 public API
%% @end
%%%-------------------------------------------------------------------

-module(day_23_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    day_23_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================