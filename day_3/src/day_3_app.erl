%%%-------------------------------------------------------------------
%% @doc day_3 public API
%% @end
%%%-------------------------------------------------------------------

-module(day_3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    day_3_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================