# erl_id3_dectree

## About

Implementation of the ID3 Decision tree Algorithm in Erlang.

Classifies a dataset of playing/not-playing tennis based on weather conditions.

## How to run

```erlang
c(erl_id3_dectree).
Examples = erl_id3_dectree:examples_play_tennis().
Tree = erl_id3_dectree:learn_tree(Examples).
erl_id3_dectree:pretty_print(Tree).
erl_id3_dectree:generalize(Tree, [{outlook, overcast}, {temperature, hot}, {humidity, high}, {windy, true}]).
```

## License

MIT

## Author

Kim Hammar <kimham@kth.se>

2017
