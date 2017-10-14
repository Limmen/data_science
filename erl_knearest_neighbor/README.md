# erl_knearest_neighbor

## About
Implementation of K-Nearest Neighbor algorithm in Erlang.

Classifies a dataset of playing/not-playing tennis based on weather conditions.

## How to run

```erlang
c(erl_knearest_neighbor).
Examples = erl_knearest_neighbor:examples_play_tennis().
erl_knearest_neighbor:classify(4, [{outlook, overcast}, {temperature, hot}, {humidity, high}, {windy, true}], Examples).
erl_knearest_neighbor:classify(4, [{outlook, rain}, {windy, true}], Examples).
erl_knearest_neighbor:classify(4, [{outlook, rain}], Examples).
```

## License

MIT

## Author

Kim Hammar <kimham@kth.se>

2017
