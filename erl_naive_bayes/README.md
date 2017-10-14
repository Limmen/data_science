# erl_naive_bayes

## About

Implementation of the Naive Bayes Classifier in Erlang.

Classifies a dataset of playing/not-playing tennis based on weather conditions.

## How to run

```erlang
c(erl_naive_bayes).
Examples = erl_naive_bayes:examples_play_tennis().
Model = erl_naive_bayes:learn_model(Examples).
erl_naive_bayes:classify(Model, [{outlook, overcast}, {temperature, hot}, {humidity, high}, {windy, true}]).
erl_naive_bayes:classify(Model, [{outlook, overcast}]).
erl_naive_bayes:classify(Model, [{outlook, sunny}, {humidity, high}]).
```

## License

MIT

## Author

Kim Hammar <kimham@kth.se>

2017
