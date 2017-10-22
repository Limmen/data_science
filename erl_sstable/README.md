# erl_sstable

## About 
Implementation of simple un-optimized SSTable + MemTable storage in Erlang.

## How to run

```erlang
c(erl_sstable).
erl_sstable:start().
erl_sstable:store_put_many([{"1", "2"}, {"2", "3"}, {"3", "4"}, {"5", "6"}, {"6", "7"}, {"7", "8"}, {"1", "3"}, {"1", "4"}, {"2", "100"}, {"11", "11"}, {"gurka", "1"}, {"15", "17"}, {"22", "29"}, {"23", "29"}, {"11", "15"}, {"31", "32"}]).
{ok,WAL} = dets:open_file("write_ahead_log",[{type, set}]), dets:traverse(WAL, fun(X) -> {continue, X} end).
erl_sstable:debug().
erl_sstable:store_get("1").
erl_sstable:store_get("2").
erl_sstable:store_get("5").
```

## License

MIT

## Author 

Kim Hammar <kimham@kth.se>

2017