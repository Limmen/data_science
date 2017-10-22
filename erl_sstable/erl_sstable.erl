%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_kmeans.erl
%% Simple SSTable implementation, using erlang Maps for MemTable and DETS for SSTable storage.
%% Does not use index in SSTables. SSTables work as follows: Writes go to RAM storage which is fast,
%% and operations are logged to WAL on disk for recovery in case of failure.
%% I.e increasing write-througput without sacrificing reliability. When MemTable reaches a
%% certain size it is compacted into an SSTable and stored on disk in sorted table format of (stringKey->stringVal).
%% After migrating the memtable to disk the new memtable is empty.
%% To read a key, first lookup in MemTable, if miss, lookup in latest SSTable, then second latest and so on...
%% Since SSTables are stored sorted and with a inddex, a lookup is just 1 disk-seek.
%% To optimize for read-througput MemTable can include a bloom filter, which allows to quickly check if key is present or not
%% instead of having to loop through all SSTables on disk.
%% Periodically a Merge-compaction happen where all SSTables on disk are merged into one and deleted keys are removed.
%% Test usage:
%% c(erl_sstable).
%% > erl_sstable:start().
%% > erl_sstable:store_put_many([{"1", "2"}, {"2", "3"}, {"3", "4"}, {"5", "6"}, {"6", "7"}, {"7", "8"}, {"1", "3"}, {"1", "4"}, {"2", "100"}, {"11", "11"}, {"gurka", "1"}, {"15", "17"}, {"22", "29"}, {"23", "29"}, {"11", "15"}, {"31", "32"}]).
%% > {ok,WAL} = dets:open_file("write_ahead_log",[{type, set}]), dets:traverse(WAL, fun(X) -> {continue, X} end).
%% > erl_sstable:debug().
%% > erl_sstable:store_get("1").
%% > erl_sstable:store_get("2").
%% > erl_sstable:store_get("5").
%% @end
%%%-------------------------------------------------------------------
-module(erl_sstable).
-author('Kim Hammar <kimham@kth.se>').

%% API

-export([start/0, store_put/2, store_get/1, store_delete/1, store_put_many/1, debug/0]).

%% macros

-define(COMPACTION, 30000). %%ms
-define(MEMTABLE_SIZE, 5). %%items
-define(WAL, "write_ahead_log"). %%log name

%% types

-type operation():: {id: integer(), {Operation::op(), Key::string()} | {Operation::op(), Key::string(), Value::string()}}.

-type op():: put | delete.

-type sstables() :: list(TableName::string()).

-type memtable() :: map().

-type query_result() :: {string(), string()} | not_found | ok | timeout.

%%====================================================================
%% API functions
%%====================================================================

%% Put many K-V pairs at once, return list of results
-spec store_put_many(list({string(), string()})) -> list(query_result()).
store_put_many(KVs)->
    lists:map(fun({K,V}) -> store_put(K,V) end, KVs).

%% Put a K-V pair
-spec store_put(Key::string(), Value::string()) -> query_result().
store_put(Key, Value)->
    storage ! {self(), {put, Key, Value}},
    receive_queryresult().

%% Get a key
-spec store_get(Key::string()) -> query_result().
store_get(Key)->
    storage ! {self(), {get, Key}},
    receive_queryresult().

%% Delete a key
-spec store_delete(Key::string()) -> query_result().
store_delete(Key)->
    storage ! {self(), {delete, Key}},
    receive_queryresult().

%% Debug, print MemTable and SSTables
-spec debug() -> ok.
debug()->
    storage ! debug,
    ok.

%% Start storage
-spec start()-> ok.
start()->
    register(storage, spawn(fun()-> init() end)),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% Helper function to wait for storage response to query.
-spec receive_queryresult() -> query_result().
receive_queryresult()->
    receive
	X ->
	    X
    after
	1000 ->
	    timeout
    end.

%% Initialize storage
-spec init() -> ok.
init()->
    timer:send_interval(?COMPACTION, compaction),
    new_wal(),
    loop(new_memtable(), [], 0).

%% Main loop, receive client requests and perform periodic compaction
-spec loop(MemTable::memtable(), SSTables::sstables(), N::integer()) -> any().
loop(MemTable, SSTables, N)->
    receive
	compaction ->
	    io:format("Compaction ~n"),
	    SSTables1 = compact(SSTables),
	    loop(MemTable, SSTables1, N);
	{Sender, {put, K,V}} ->
	    persist_operation({N+1, {put, K,V}}),
	    MemTable1 = maps:put(K, V, MemTable),
	    {MemTable2, SSTables1, N1} = check_memtable_size(MemTable1, SSTables, N),
	    Sender ! ok,
	    loop(MemTable2, SSTables1, N1+1);
	{Sender, {get, K}} ->
	    Res = read_storage(K, SSTables, MemTable),
	    Sender ! Res,
	    loop(MemTable, SSTables, N);
	{Sender, {delete, K}} ->
	    persist_operation({N+1, {delete, K}}),
	    MemTable1 = maps:put(K, "deleted", MemTable),
	    Sender ! ok,
	    loop(MemTable1, SSTables, N+1);
	debug ->
	    io:format("MemTable: ~p~nSStables: ~p~n", [MemTable,SSTables]),
	    loop(MemTable, SSTables, N)
    end.

%% Check if memtable size is over threshold for creating SSTable
-spec check_memtable_size(MemTable::memtable(), SSTables::sstables(), N::integer())-> {memtable(), sstables()}.
check_memtable_size(MemTable, SSTables, N)->
    case maps:size(MemTable) of
	X when X > ?MEMTABLE_SIZE ->
	    io:format("Migrating Memtable to SSTable on disk~n"),
	    Name = "sstable" ++ integer_to_list(length(SSTables) + 1),
	    new_sstable(Name, MemTable),
	    new_wal(),
	    {new_memtable(), [Name|SSTables], 0};
	X when X =< ?MEMTABLE_SIZE ->
	    {MemTable, SSTables, N}
    end.

%% Create new SSTable
-spec new_sstable(Name::string(), MemTable::map()) -> SSTableName::string().
new_sstable(Name, MemTable)->
    persist_memtable(Name, MemTable).

%% Create new Memtable
-spec new_memtable() -> MemTable::map().
new_memtable()->
    #{}.

%% Create new WAL
-spec new_wal() -> ok.
new_wal()->
    {ok,Ref}=dets:open_file(?WAL,[{type, set}]),
    dets:delete_all_objects(Ref),
    dets:close(Ref),
    ok.

%% Read Merged view of MemTable and SSTables (Can be optimized with Bloomfilter)
-spec read_storage(Key::string(), SSTables::sstables(), MemTable::memtable()) -> {string(), string()} | not_found.
read_storage(Key, SSTables, MemTable)->
    case maps:is_key(Key, MemTable) of
	true ->
	    case maps:get(Key, MemTable) of
		"deleted" ->
		    not_found;
		X ->
		    {Key, X}
	    end;
	false ->
	    read_sstables(Key, SSTables)
    end.

%% Read SSTables
-spec read_sstables(Key::string(), SSTables::sstables()) -> {string(), string()} | not_found.
read_sstables(_, [])->
    not_found;
read_sstables(Key, [H|T]) ->
    case read_sstable(Key, H) of
	not_found ->
	    read_sstables(Key, T);
	{Key, "deleted"} ->
	    not_found;
	X ->
	    X
    end.

%% Read single SSTable
-spec read_sstable(Key::string(), SSTable::string()) -> {string(), string()} | not_found.
read_sstable(Key, SSTable)->
    {ok,Ref} = dets:open_file(SSTable,[{type, set}]),
    case dets:lookup(Ref, Key) of
	[X] ->
	    dets:close(Ref),
	    X;
	[] ->
	    dets:close(Ref),
	    not_found
    end.

%% Persist MemTable as a new SSTable
-spec persist_memtable(FileName::string(), MemTable::memtable())-> FileName::string().
persist_memtable(FileName, Data) ->
    {ok,Ref}=dets:open_file(FileName,[{type, set}]),
    dets:insert(Ref, lists:keysort(1, maps:to_list(Data))),
    dets:close(Ref),
    FileName.

%% Persist a operation in WAL
-spec persist_operation(Operation::operation()) -> ok.
persist_operation(Operation)->
    {ok,Ref}=dets:open_file(?WAL,[{type, set}]),
    dets:insert(Ref, Operation),
    dets:close(Ref),
    ok.

%% Delete a file
-spec delete_file(FileName::string())-> ok | {error, Reason:: file:posix() | badarg}.
delete_file(FileName)->
    file:delete(FileName).

%% Compact SSTables into a single SSTable
-spec compact(sstables()) -> CompactedTables::sstables().
compact(SSTables)->
    NewSSTableState = lists:foldl(fun(T, State) ->
					  {ok,Ref}=dets:open_file(T,[{type, set}]),
					  KeyVals = dets:traverse(Ref, fun(X) -> {continue, X} end),
					  dets:close(Ref),
					  State1 = lists:foldl(fun({K,V}, Acc) -> maps:put(K,V,Acc) end, State, KeyVals),
					  State1
				  end, #{}, lists:reverse(SSTables)),
    lists:map(fun(T) -> delete_file(T) end, SSTables),
    NewSSTableState1 = maps:filter(fun(_,V) -> V /= "deleted" end, NewSSTableState),
    Name = "sstable1",
    {ok,Ref}=dets:open_file(Name,[{type, set}]),
    dets:insert(Ref, lists:keysort(1, maps:to_list(NewSSTableState1))),
    dets:close(Ref),
    [Name].
