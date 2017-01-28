-module(hash_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, hash_at/1]).
-behaviour(gen_server).


generate_hash(_Salt, _Index, Hash, 2017) -> Hash;
generate_hash(Salt, Index, Hash, Count) -> 
    NewHash = md5:md5_hex(Hash),
    generate_hash(Salt, Index, NewHash, Count + 1).

generate_hash(Salt, Index) ->
    Hash = md5:md5_hex(Salt ++ integer_to_list(Index)),
    generate_hash(Salt, Index, Hash, 1).

start(Salt) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Salt], []).
hash_at(Index) -> gen_server:call(?MODULE, {hash, Index}).

init([Salt]) ->
    {ok, {Salt, #{}}}.

handle_call({hash, Index}, _From, {Salt, Hashes}) -> 
    NewHashes = case maps:is_key(Index, Hashes) of
        false -> Hash = generate_hash(Salt, Index),
                 Hashes#{Index => Hash};
        _ -> Hashes
    end,

    #{Index := TheHash} = NewHashes,

    {reply, TheHash, {Salt, NewHashes}}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
