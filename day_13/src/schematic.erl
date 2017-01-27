-module(schematic).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, element_at/3]).
-behaviour(gen_server).

-import(schematic_impl, [schematic_element/3]).

start(FavoriteNumber) -> gen_server:start_link(?MODULE, [FavoriteNumber], []).
element_at(Schematic, X, Y) -> gen_server:call(Schematic, {element, X, Y}).

init([FavoriteNumber]) ->
    {ok, {FavoriteNumber}}.

handle_call({element, X, Y}, _From, {FavoriteNumber}=State) -> 
    Element = schematic_element(X, Y, FavoriteNumber),
    {reply, Element, State}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
